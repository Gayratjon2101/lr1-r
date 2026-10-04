# ============================================================================
# Лабораторная работа №1. Блок 3.
# Файл: 03_parsing.R  —  Парсинг данных из Интернета (API hh.ru)
#
# ЗАДАНИЕ 4: аналогичным образом произвести парсинг сайта,
# выбрав из предложенных: справочники Банка данных заработных плат hh.ru,
# метод get-salary-industries (отрасли и сферы деятельности).
#
# Создаваемые файлы:
#   output/raw_salary_industries.json  — сырой ответ сервера
#   output/hh_salary_industries.xlsx   — обработанные таблицы
# ============================================================================

library(httr)      # HTTP-запросы с заголовками и кодами ответа
library(jsonlite)  # разбор JSON
library(dplyr)     # обработка и агрегация, как в примере методички
library(openxlsx)  # сохранение результата


# ============================================================================
# ЧАСТЬ А. Почему пример из методички не воспроизводится
# ============================================================================
# Пример в методичке скачивает вакансии с https://api.hh.ru/vacancies.
# Проверка показала: сейчас этот адрес отвечает кодом 403 (forbidden).
# hh.ru закрыл поиск вакансий для запросов без зарегистрированного
# приложения. Убеждаемся в этом воспроизводимо, а не на словах:

demo <- tryCatch(
  fromJSON("https://api.hh.ru/vacancies?text=r"),
  error = function(e) paste("Ошибка:", conditionMessage(e))
)
cat("--- А. Пример из методички сейчас возвращает ---\n")
print(if (is.character(demo)) demo else "данные получены (доступ снова открыт)")

# Помимо закрытого доступа, в коде примера есть собственные ошибки,
# которые стоит знать для защиты:
#  1. В цикле for (pageNum in 0:20) переменная pageNum нигде не подставлена
#     в адрес, поэтому 21 раз скачивается одна и та же первая страница.
#     Правильно: paste0("https://api.hh.ru/vacancies?text=r&page=", pageNum)
#  2. В адресе ?text=\"r кавычка открыта и не закрыта.
#  3. Пустой vacanciesdf создаётся со столбцами Name, Currency, From, Area,
#     Requerement, а в rbind подаются столбцы с другими именами.
#     rbind сопоставляет столбцы по именам, и R останавливается с ошибкой
#     "names do not match previous names".
#  4. renv::install("RCurl", "jsonlite", ...) — пакеты нужно передавать
#     одним вектором: renv::install(c("RCurl", "jsonlite", ...)).
#  5. Курсы валют (74 и 2.67) зашиты в код константами и давно устарели.


# ============================================================================
# ЧАСТЬ Б. Функция запроса к API
# ============================================================================
# Почему httr, а не fromJSON(адрес) как в методичке:
#  - API hh.ru требует заголовок HH-User-Agent с названием приложения.
#    fromJSON(адрес) не умеет передавать произвольные заголовки.
#  - httr возвращает КОД ОТВЕТА (200, 403, 404...). Без него нельзя отличить
#    «данных нет» от «нас не пустили».

UA <- "lr1-r-student/1.0"
INDUSTRIES_PATH <- "/salary_statistics/dictionaries/salary_industries"

hh_get <- function(path, query = list()) {
  resp <- GET(
    url    = paste0("https://api.hh.ru", path),
    query  = query,
    add_headers(`HH-User-Agent` = UA),
    timeout(20)
  )
  code <- status_code(resp)
  txt  <- content(resp, as = "text", encoding = "UTF-8")

  # Явная проверка: при любом коде, кроме 200, останавливаемся
  # с понятным сообщением, а не разбираем страницу с ошибкой как данные.
  if (code != 200) {
    stop(sprintf("HTTP %d: %s", code, substr(txt, 1, 200)))
  }

  list(text = txt, data = fromJSON(txt))
}


# ============================================================================
# ЧАСТЬ В. Первый запрос и структура ответа
# ============================================================================
res_ru <- hh_get(INDUSTRIES_PATH, query = list(locale = "RU", host = "hh.ru"))

# Сохраняем сырой ответ: если hh завтра изменит API или закроет доступ,
# у нас останется исходник, на котором воспроизводится весь анализ.
con <- file("output/raw_salary_industries.json", encoding = "UTF-8")
writeLines(res_ru$text, con)
close(con)

sectors <- res_ru$data

cat("\n--- В. Структура ответа ---\n")
cat("Класс объекта:", class(sectors), "\n")
cat("Отраслей верхнего уровня:", nrow(sectors), "\n")
str(sectors, max.level = 1)

# fromJSON превратил JSON в дата-фрейм, где столбец industries — это
# СПИСОК дата-фреймов: у каждой отрасли свой вложенный список подотраслей.
# Такой формат неудобен для анализа, его нужно «развернуть» в плоскую таблицу.
stopifnot(is.data.frame(sectors), "industries" %in% names(sectors))


# ============================================================================
# ЧАСТЬ Г. Разворачиваем дерево в плоскую таблицу
# ============================================================================
# Было:  1 строка = 1 отрасль, внутри вложенный список подотраслей
# Стало: 1 строка = 1 подотрасль + столбцы с её родительской отраслью

flatten_industries <- function(sectors) {
  rows <- lapply(seq_len(nrow(sectors)), function(i) {
    sub <- sectors$industries[[i]]

    # Если у отрасли нет подотраслей, не теряем её, а сохраняем с NA
    if (is.null(sub) || NROW(sub) == 0) {
      return(data.frame(
        sector_id     = sectors$id[i],
        sector_name   = sectors$name[i],
        industry_id   = NA_character_,
        industry_name = NA_character_
      ))
    }

    data.frame(
      sector_id     = sectors$id[i],
      sector_name   = sectors$name[i],
      industry_id   = as.character(sub$id),
      industry_name = sub$name
    )
  })
  do.call(rbind, rows)
}

ru <- flatten_industries(sectors)

cat("\n--- Г. Плоская таблица ---\n")
cat("Строк (подотраслей):", nrow(ru), "\n")
print(head(ru, 10))


# ============================================================================
# ЧАСТЬ Д. Цикл запросов — аналог цикла по страницам из методички
# ============================================================================
# В методичке цикл перебирал страницы выдачи. У справочника страниц нет,
# зато есть параметр host — сайт группы hh в разных странах
# (документация: hh.ru, rabota.by, hh1.az, hh.uz, hh.kz, headhunter.ge,
#  headhunter.kg). Перебираем их и проверяем, един ли справочник.
#
# Каждый запрос завёрнут в tryCatch: если один сайт не ответит,
# цикл продолжится, а не упадёт целиком.

hosts <- c("hh.ru", "hh.kz", "hh.uz", "rabota.by")

all_list <- list()
for (h in hosts) {
  res <- tryCatch(
    hh_get(INDUSTRIES_PATH, query = list(locale = "RU", host = h)),
    error = function(e) e
  )

  if (inherits(res, "error")) {
    cat("Сайт", h, "— пропущен:", conditionMessage(res), "\n")
    next
  }

  df <- flatten_industries(res$data)
  df$host <- h
  all_list[[h]] <- df
  cat("Сайт", h, "— загружено подотраслей:", nrow(df), "\n")

  Sys.sleep(1)  # пауза между запросами, чтобы не нагружать сервер
}

all_hosts <- do.call(rbind, all_list)
rownames(all_hosts) <- NULL


# ============================================================================
# ЧАСТЬ Е. Обработка и анализ — по аналогии с group_by/summarise из методички
# ============================================================================

# Е.1. Сколько подотраслей в каждой отрасли (по hh.ru)
by_sector <- ru %>%
  group_by(sector_name) %>%
  summarise(n_industries = sum(!is.na(industry_id))) %>%
  arrange(desc(n_industries))

cat("\n--- Е.1. Число подотраслей по отраслям ---\n")
print(by_sector, n = Inf)

# Е.2. Проверка качества данных: уникальны ли идентификаторы подотраслей?
# Если один id встречается в двух отраслях — справочник не строгое дерево.
dup_ids <- ru %>%
  filter(!is.na(industry_id)) %>%
  count(industry_id, name = "n") %>%
  filter(n > 1)

cat("\n--- Е.2. Повторяющиеся id подотраслей:", nrow(dup_ids), "---\n")
if (nrow(dup_ids) > 0) print(dup_ids)

# Е.3. Сравнение сайтов: совпадает ли справочник в разных странах
by_host <- all_hosts %>%
  group_by(host) %>%
  summarise(
    sectors    = n_distinct(sector_id),
    industries = sum(!is.na(industry_id)),
    same_as_ru = setequal(industry_id, ru$industry_id)
  )

cat("\n--- Е.3. Сравнение справочника по сайтам ---\n")
print(by_host)

# Е.4. Пример выборки: все подотрасли одной отрасли
logistics <- ru %>%
  filter(grepl("логистик", sector_name, ignore.case = TRUE)) %>%
  select(industry_id, industry_name)

cat("\n--- Е.4. Подотрасли отрасли «Перевозки, логистика, склад, ВЭД» ---\n")
print(logistics)


# ============================================================================
# ЧАСТЬ Ж. Параметр locale: тот же справочник на английском + соединение
# ============================================================================
# Параметр locale из документации меняет язык названий.
# Скачиваем английскую версию и соединяем с русской по id подотрасли —
# это демонстрация left_join, главной операции объединения таблиц.

en <- tryCatch(
  flatten_industries(hh_get(INDUSTRIES_PATH,
                            query = list(locale = "EN", host = "hh.ru"))$data),
  error = function(e) NULL
)

if (!is.null(en)) {
  bilingual <- ru %>%
    filter(!is.na(industry_id)) %>%
    select(industry_id, sector_name, industry_name) %>%
    left_join(
      en %>% select(industry_id,
                    sector_name_en   = sector_name,
                    industry_name_en = industry_name),
      by = "industry_id"
    )
  cat("\n--- Ж. Двуязычный справочник (первые 10 строк) ---\n")
  print(head(bilingual, 10))
} else {
  bilingual <- data.frame(note = "Английская версия недоступна")
  cat("\nАнглийская версия справочника не загрузилась\n")
}


# ============================================================================
# ЧАСТЬ З. Сохранение результатов
# ============================================================================
write.xlsx(
  list(
    industries_ru = ru,
    by_sector     = by_sector,
    by_host       = by_host,
    bilingual     = bilingual
  ),
  file = "output/hh_salary_industries.xlsx",
  overwrite = TRUE
)

cat("\nСохранено: output/raw_salary_industries.json\n")
cat("Сохранено: output/hh_salary_industries.xlsx (4 вкладки)\n")
