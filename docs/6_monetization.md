# Метрики монетизации pt2 {#c3_monetization2}

## Запись занятия

<iframe width="560" height="315" src="https://www.youtube.com/embed/NgqD-mPtR00?si=xFVEdNmICxbq2iT_" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" referrerpolicy="strict-origin-when-cross-origin" allowfullscreen></iframe>

## Папка с записями в Dropbox

https://www.dropbox.com/scl/fo/pzw6egi6notlmr08vq1wq/ACf8iAANpusI3UcBlOFXoKM?rlkey=y9c0mao5qj2r85q1cgrdrg4oc&st=328vy5sx&dl=0

## Код занятия на Python
https://colab.research.google.com/drive/1zpDnpWjJIA_PoYh8hOlbPqMwGEHkL53a?usp=sharing

## Расчет метрик

### Conversion

Задача: для июньской когорты посчитать конверсию за первые семь дней жизни в приложении (lifetime <= 7). Сделать это в разбивке по платформам.

- Инсталлы: https://gitlab.com/hse_mar/mar211f/-/raw/main/data/installs.csv
- Платежи: https://gitlab.com/hse_mar/mar211f/-/raw/main/data/payments_custom.csv 


Решение:


``` r
# Подключаем библиотеки
library(tidyverse)
library(plotly)

# Загружаем данные
installs <- read_csv("./data/installs.csv")
payments <- read_csv("./data/payments_custom.csv")

# Выделяем инсталлы в июне
installs_june <- installs %>%
  filter(dt >= "2022-06-01", dt < "2022-07-01")

# Считаем количество пользователей по платформам
installs_june_stat <- installs_june %>%
  group_by(platform) %>%
  summarise(total_users = n_distinct(user_pseudo_id), .groups = "drop")

# Объединяем платежи с инсталлами за июнь
payments_june <- payments %>%
  inner_join(installs_june, by = c("user_pseudo_id", "platform")) %>%
  mutate(lifetime = as.integer(as.Date(pay_dt) - as.Date(dt)))

# Фильтруем только платежи в первые 7 дней
payments_june_stat <- payments_june %>%
  filter(lifetime %in% 0:7) %>%
  group_by(platform) %>%
  summarise(
    n_payers = n_distinct(user_pseudo_id),
    gross = sum(gross, na.rm = TRUE),
    n_transactions = n()
  )

# Объединяем статистику установок и платежей
monetization_june <- installs_june_stat %>%
  left_join(payments_june_stat, by = "platform") %>%
  mutate(conversion = round(n_payers / total_users, 3))

kableExtra::kable(monetization_june)
```



|platform | total_users| n_payers|    gross| n_transactions| conversion|
|:--------|-----------:|--------:|--------:|--------------:|----------:|
|ANDROID  |       77770|      894| 13411.37|           1882|      0.011|
|IOS      |       33010|     1165| 29107.09|           3335|      0.035|

### ARPU

Задача: посчитать и нарисовать кумулятивное ARPU

Логика расчета:

- группировка по лайфтайму (сколько заплатили в каждый день от инсталла)
- кумулятивная сумма (сколько накопительно заплатили к каждому дню лайфтайма)
- делим кум.сумму на всего пользователей
- рисуем график

Решение:


``` r
# Фильтруем платежи за первые 30 дней
payments_june_stat <- payments_june %>%
  filter(lifetime %in% 0:30) %>%
  group_by(platform, lifetime) %>%
  summarise(gross = sum(gross, na.rm = TRUE), .groups = "drop") %>%
  arrange(platform, lifetime)

# Считаем кумулятивную сумму
payments_june_stat <- payments_june_stat %>%
  group_by(platform) %>%
  mutate(
    gross_cum = cumsum(gross),
    gross_cum2 = cumsum(gross) # Дублирует предыдущую колонку, можно убрать
  ) %>%
  ungroup()

# Объединяем с количеством установок
payments_june_stat <- payments_june_stat %>%
  left_join(installs_june_stat, by = "platform") %>%
  mutate(cARPU = gross_cum / total_users)

plot_ly(payments_june_stat, x = ~lifetime, y = ~cARPU, color = ~platform, type = 'scatter', mode = 'lines') %>%
  add_lines(x = c(0, 30), y = 0.4, line = list(color = "red"), inherit = FALSE, showlegend = FALSE) %>%
  layout(
    title = "cARPU, lifetime < 30, июньская когорта"
  )
```

```
## Warning in RColorBrewer::brewer.pal(N, "Set2"): minimal value for n is 3, returning requested palette with 3 different levels
## Warning in RColorBrewer::brewer.pal(N, "Set2"): minimal value for n is 3, returning requested palette with 3 different levels
```

```{=html}
<div class="plotly html-widget html-fill-item" id="htmlwidget-6a8465376400115d7be8" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-6a8465376400115d7be8">{"x":{"visdat":{"3c0581dfa5ee3":["function () ","plotlyVisDat"]},"cur_data":"3c0581dfa5ee3","attrs":{"3c0581dfa5ee3":{"x":{},"y":{},"mode":"lines","color":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter"},"3c0581dfa5ee3.1":{"x":[0,30],"y":0.40000000000000002,"type":"scatter","mode":"lines","line":{"color":"red"},"showlegend":false,"inherit":false}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"cARPU, lifetime < 30, июньская когорта","xaxis":{"domain":[0,1],"automargin":true,"title":"lifetime"},"yaxis":{"domain":[0,1],"automargin":true,"title":"cARPU"},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30],"y":[0.050456731387424458,0.08558235823582358,0.1083051305130513,0.1242025202520252,0.13626976983412628,0.15430885945737433,0.16511148257682912,0.17244914491449145,0.1845049504950495,0.19628892889288929,0.20483194033689084,0.21269191204834767,0.22181509579529382,0.22922720843512923,0.23589108910891088,0.24254314002828856,0.25023672367236721,0.25614620033431917,0.26200411469718404,0.26646148900604344,0.27364433586215764,0.27815365822296512,0.28280159444515879,0.28873408769448372,0.29365500835797864,0.2978835026359779,0.30435707856499933,0.30994535167802495,0.31546727529895846,0.32118426128327121,0.32727491320560631],"mode":"lines","type":"scatter","name":"ANDROID","marker":{"color":"rgba(102,194,165,1)","line":{"color":"rgba(102,194,165,1)"}},"textfont":{"color":"rgba(102,194,165,1)"},"error_y":{"color":"rgba(102,194,165,1)"},"error_x":{"color":"rgba(102,194,165,1)"},"line":{"color":"rgba(102,194,165,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30],"y":[0.20735534686458648,0.3711384428960921,0.49829687973341408,0.58943532262950626,0.6719612238715541,0.74316782793093006,0.80922296273856409,0.88176582853680707,0.9429475916388973,1.0003792790063619,1.0544265374129052,1.1007440169645561,1.1462602241744926,1.1887991517721903,1.2409094213874583,1.2922714328991214,1.3388388367161466,1.3810754316873675,1.4255583156619207,1.4713835201454106,1.5208936685852772,1.5704883368676159,1.6211535898212663,1.6673044531960013,1.708581036049682,1.7502217509845501,1.795467131172372,1.8378385337776433,1.8704931838836718,1.9223423205089367,1.9518149045743716],"mode":"lines","type":"scatter","name":"IOS","marker":{"color":"rgba(141,160,203,1)","line":{"color":"rgba(141,160,203,1)"}},"textfont":{"color":"rgba(141,160,203,1)"},"error_y":{"color":"rgba(141,160,203,1)"},"error_x":{"color":"rgba(141,160,203,1)"},"line":{"color":"rgba(141,160,203,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[0,30],"y":[0.40000000000000002,0.40000000000000002],"type":"scatter","mode":"lines","line":{"color":"red"},"showlegend":false,"marker":{"color":"rgba(44,160,44,1)","line":{"color":"rgba(44,160,44,1)"}},"error_y":{"color":"rgba(44,160,44,1)"},"error_x":{"color":"rgba(44,160,44,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

### Воронка платежей

Задача: посчитать и нарисовать воронку платежей


Логика расчета:
- отсортировать табличку payments_june по пользователю и полю ts
- создать номер платежа каждого пользователя, cumcount()
- посчитать, сколкьо пользователей сделало 1, 2...10 платежей
- посчитать долю от всего пользователей
- нарисовать


``` r
# Сортируем платежи по пользователю и времени транзакции
payments_june <- payments_june %>%
  arrange(user_pseudo_id, ts) %>%
  group_by(user_pseudo_id) %>%
  mutate(payment_number = row_number()) %>%
  ungroup()

# Фильтруем только первые 10 платежей
payment_funnel <- payments_june %>%
  filter(payment_number <= 10) %>%
  group_by(payment_number) %>%
  summarise(n_users = n_distinct(user_pseudo_id), .groups = "drop")

# Добавляем общее количество пользователей и считаем долю
total_users <- payments_june %>% summarise(n_distinct(user_pseudo_id)) %>% pull()

payment_funnel <- payment_funnel %>%
  mutate(
    total_users = total_users,
    share = n_users / total_users
  )

# Строим воронку платежей
plot_ly(payment_funnel,  x = ~payment_number,  y = ~share,  type = "bar") %>%
  layout(title = "Воронка платежей")
```

```{=html}
<div class="plotly html-widget html-fill-item" id="htmlwidget-fe2cf739e8e7d75fa068" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-fe2cf739e8e7d75fa068">{"x":{"visdat":{"3c05820391a90":["function () ","plotlyVisDat"]},"cur_data":"3c05820391a90","attrs":{"3c05820391a90":{"x":{},"y":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"Воронка платежей","xaxis":{"domain":[0,1],"automargin":true,"title":"payment_number"},"yaxis":{"domain":[0,1],"automargin":true,"title":"share"},"hovermode":"closest","showlegend":false},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":[1,2,3,4,5,6,7,8,9,10],"y":[1,0.55310547592796766,0.38478500551267913,0.29988974641675853,0.23594266813671444,0.2036016170525542,0.17236310180080852,0.15104740904079383,0.12936420433664095,0.11466372657111357],"type":"bar","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

## Домашнее задание

### level 1 (IATYTD)

Внимательно разберите решения заданий (материалы конспекта).


<!-- ### level 2 (HNTR) -->

<!-- ### level 3 (HMP) -->

<!-- ### level 4 (UV) -->

<!-- ### level 5 (N) -->


### level 2 (HNTR)

На основе данных по [платежам](https://gitlab.com/hse_mar/mar211f/-/raw/main/data/payments_custom.csv) нарисуйте [area plot](https://en.wikipedia.org/wiki/Area_chart) подневную структуру гросса проекта, в котором цветами выделите группы пользователей по количеству дней с момента инсталла:

-   группа 1: 0 дней с инсталла

-   группа 2: 1-7 дней с момента инсталла

-   группа 3: 8-28 дней с инсталла

-   группа 4: более 28 дней с инсталла

Решение аналогично такому же заданию на расчет структуры DAU.


### level 3 (HMP)

Сделайте табличку монетизации для июньских и июльских когорт пользователей.
В табличке должны быть следующие метрики: 

- total_users: всего пользователей когорты
- n_payers: количество платящих
- conversion: конверсия в платящих
- gross: сколько всего заплатили
- ARPU
- ARPPU
- AOV (average order value): размер среднего платежа (средний чек)
- AvPurchases: среднее количество платежей на пользователя

По строкам -- пользователи, пришедшие в июне и июле, две строки.

### level 4 (UV)

Постройте воронку платежей (возьмите первые 10 платежей) в когорте июньских пользователей, с разбивкой по платформам.
Попробуйте сделать два графика -- один с долями от первого шага, другой -- с долями от предыдущего шага.

Для обращения к значению в предыдущей ячейки питонистам поможет метод `shift()`. Тем, кто пишет на tidyverse R -- функция `lag()`.


### level 5 (N)

Постройте график накопительной конверсии в когорте июньских пользователей с разбивкой по источнику (media_source) пользователей.

Для этого надо сначала посчитать, в какой день от инсталла пользователь сделал первый платеж (lifetime).
Потом посчитать, сколько пользователей сделало первый платеж в 0-30 дни от инсталла (new payers).
Посчитать накопительную сумму по количеству пользователей (cumulative new payers).
Посчитать отношение cumulative new payers / total users
Нарисовать график.





