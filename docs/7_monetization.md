# Метрики монетизации pt3 {#c3_monetization3}

## Код занятия на Python
https://colab.research.google.com/drive/13AezTnu5f73Ex-kRbefOKe0l1Yajp0q3?usp=sharing

## Разбор домашнего задания


``` r
# Подключаем библиотеки
library(tidyverse)
library(plotly)
library(kableExtra)

# Загружаем данные
installs <- read_csv("./data/installs.csv")
payments <- read_csv("./data/payments_custom.csv")

# Выделяем инсталлы в июне
installs_june <- installs %>%
  filter(dt >= "2022-06-01", dt < "2022-07-01")
```



### level 2 (HNTR)

На основе данных по [платежам](https://gitlab.com/hse_mar/mar211f/-/raw/main/data/payments_custom.csv) нарисуйте [area plot](https://en.wikipedia.org/wiki/Area_chart) подневную структуру гросса проекта, в котором цветами выделите группы пользователей по количеству дней с момента инсталла:

-   группа 1: 0 дней с инсталла

-   группа 2: 1-7 дней с момента инсталла

-   группа 3: 8-28 дней с инсталла

-   группа 4: более 28 дней с инсталла

Решение аналогично такому же заданию на расчет структуры DAU.

Решение:



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


``` r
# Считаем количество пользователей по платформам
installs_m <- installs %>%
  filter(dt >= "2022-06-01", dt < "2022-07-25") %>%
  mutate(month = month(dt, label = TRUE, locale = 'en_US.UTF-8', abbr = FALSE))

installs_m_stat <- installs_m %>%
  group_by(month) %>% 
  summarise(total_users = n_distinct(user_pseudo_id), .groups = "drop")

# Объединяем платежи с инсталлами за июнь
payments_m <- payments %>%
  inner_join(installs_m, by = c("user_pseudo_id", "platform")) %>%
  mutate(lifetime = as.integer(as.Date(pay_dt) - as.Date(dt)))
```

```
## Warning in inner_join(., installs_m, by = c("user_pseudo_id", "platform")): Detected an unexpected many-to-many relationship between `x` and `y`.
## ℹ Row 1199 of `x` matches multiple rows in `y`.
## ℹ Row 106046 of `y` matches multiple rows in `x`.
## ℹ If a many-to-many relationship is expected, set `relationship =
##   "many-to-many"` to silence this warning.
```

``` r
# Фильтруем только платежи в первые 7 дней
payments_m_stat <- payments_m %>%
  filter(lifetime %in% 0:7) %>%
  group_by(month) %>%
  summarise(
    n_payers = n_distinct(user_pseudo_id),
    gross = sum(gross, na.rm = TRUE),
    n_transactions = n()
  )

# Объединяем статистику установок и платежей
monetization_m <- installs_m_stat %>%
  left_join(payments_m_stat, by = 'month') %>%
  mutate(
    conversion = round(n_payers / total_users, 3),
    ARPU = round(gross / total_users, 3),
    ARPPU = round(gross / n_payers, 1),
    AOV = round(gross / n_transactions, 1),
    AvPurchases = round(n_transactions / n_payers, 1)
  )

kable(monetization_m) %>%
  kable_styling(font_size = 10)
```

<table class="table" style="font-size: 10px; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> month </th>
   <th style="text-align:right;"> total_users </th>
   <th style="text-align:right;"> n_payers </th>
   <th style="text-align:right;"> gross </th>
   <th style="text-align:right;"> n_transactions </th>
   <th style="text-align:right;"> conversion </th>
   <th style="text-align:right;"> ARPU </th>
   <th style="text-align:right;"> ARPPU </th>
   <th style="text-align:right;"> AOV </th>
   <th style="text-align:right;"> AvPurchases </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> June </td>
   <td style="text-align:right;"> 110780 </td>
   <td style="text-align:right;"> 2059 </td>
   <td style="text-align:right;"> 42518.46 </td>
   <td style="text-align:right;"> 5217 </td>
   <td style="text-align:right;"> 0.019 </td>
   <td style="text-align:right;"> 0.384 </td>
   <td style="text-align:right;"> 20.7 </td>
   <td style="text-align:right;"> 8.1 </td>
   <td style="text-align:right;"> 2.5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> July </td>
   <td style="text-align:right;"> 11843 </td>
   <td style="text-align:right;"> 217 </td>
   <td style="text-align:right;"> 4581.76 </td>
   <td style="text-align:right;"> 757 </td>
   <td style="text-align:right;"> 0.018 </td>
   <td style="text-align:right;"> 0.387 </td>
   <td style="text-align:right;"> 21.1 </td>
   <td style="text-align:right;"> 6.1 </td>
   <td style="text-align:right;"> 3.5 </td>
  </tr>
</tbody>
</table>



### level 4 (UV)

Постройте воронку платежей (возьмите первые 10 платежей) в когорте июньских пользователей, с разбивкой по платформам.
Попробуйте сделать два графика -- один с долями от первого шага, другой -- с долями от предыдущего шага.

Для обращения к значению в предыдущей ячейки питонистам поможет метод `shift()`. Тем, кто пишет на tidyverse R -- функция `lag()`.


``` r
users_june <- installs %>%
  filter(dt < as.Date("2022-07-01")) %>%
  distinct(user_pseudo_id) %>%
  pull(user_pseudo_id)

# Сортируем платежи по пользователю и времени транзакции
payments_june <- payments %>%
  filter(user_pseudo_id %in% users_june) %>%
  arrange(user_pseudo_id, ts) %>%
  group_by(user_pseudo_id) %>%
  mutate(payment_number = row_number()) %>%
  ungroup()

# Фильтруем только первые 10 платежей
payment_funnel <- payments_june %>%
  filter(payment_number <= 10) %>%
  group_by(platform, payment_number) %>%
  summarise(n_users = n_distinct(user_pseudo_id), .groups = "drop")

# Добавляем общее количество пользователей и считаем долю
payment_funnel <- payment_funnel %>%
  group_by(platform) %>%
  mutate(
    total_users = n_users[payment_number == 1],
    share = n_users / total_users,
    n_users_prev = lag(n_users, n = 1),
    share_prev = n_users / n_users_prev,
  )

# Строим воронку платежей
plot_ly(payment_funnel,  x = ~payment_number,  y = ~share,  type = "bar", color = ~platform) %>%
  layout(title = "Воронка платежей") %>%  
  config(displayModeBar = FALSE)
```

```
## Warning in RColorBrewer::brewer.pal(N, "Set2"): minimal value for n is 3, returning requested palette with 3 different levels
## Warning in RColorBrewer::brewer.pal(N, "Set2"): minimal value for n is 3, returning requested palette with 3 different levels
```

```{=html}
<div class="plotly html-widget html-fill-item" id="htmlwidget-bce4f39b3529c7572f96" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-bce4f39b3529c7572f96">{"x":{"visdat":{"3c1356b3d638c":["function () ","plotlyVisDat"]},"cur_data":"3c1356b3d638c","attrs":{"3c1356b3d638c":{"x":{},"y":{},"color":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"Воронка платежей","xaxis":{"domain":[0,1],"automargin":true,"title":"payment_number"},"yaxis":{"domain":[0,1],"automargin":true,"title":"share"},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"displayModeBar":false},"data":[{"x":[1,2,3,4,5,6,7,8,9,10],"y":[1,0.50125944584382875,0.33501259445843828,0.2401343408900084,0.17716204869857263,0.1486146095717884,0.12594458438287154,0.10747271200671704,0.086481947942905119,0.072208228379513018],"type":"bar","name":"ANDROID","marker":{"color":"rgba(102,194,165,1)","line":{"color":"rgba(102,194,165,1)"}},"textfont":{"color":"rgba(102,194,165,1)"},"error_y":{"color":"rgba(102,194,165,1)"},"error_x":{"color":"rgba(102,194,165,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[1,2,3,4,5,6,7,8,9,10],"y":[1,0.5934640522875817,0.42222222222222222,0.34509803921568627,0.2803921568627451,0.24509803921568626,0.20718954248366014,0.18366013071895426,0.16143790849673204,0.14640522875816994],"type":"bar","name":"IOS","marker":{"color":"rgba(141,160,203,1)","line":{"color":"rgba(141,160,203,1)"}},"textfont":{"color":"rgba(141,160,203,1)"},"error_y":{"color":"rgba(141,160,203,1)"},"error_x":{"color":"rgba(141,160,203,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

``` r
# Строим воронку платежей
plot_ly(payment_funnel,  x = ~payment_number,  y = ~share_prev,  type = "bar", color = ~platform) %>%
  layout(title = "Воронка платежей, доля от предыдущего шага") %>%  
  config(displayModeBar = FALSE)
```

```
## Warning: Ignoring 2 observations
## Warning: minimal value for n is 3, returning requested palette with 3 different levels
## Warning: minimal value for n is 3, returning requested palette with 3 different levels
```

```{=html}
<div class="plotly html-widget html-fill-item" id="htmlwidget-710479af84b11dcfa5a4" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-710479af84b11dcfa5a4">{"x":{"visdat":{"3c135710e175c":["function () ","plotlyVisDat"]},"cur_data":"3c135710e175c","attrs":{"3c135710e175c":{"x":{},"y":{},"color":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"Воронка платежей, доля от предыдущего шага","xaxis":{"domain":[0,1],"automargin":true,"title":"payment_number"},"yaxis":{"domain":[0,1],"automargin":true,"title":"share_prev"},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"displayModeBar":false},"data":[{"x":[2,3,4,5,6,7,8,9,10],"y":[0.50125944584382875,0.66834170854271358,0.71679197994987465,0.73776223776223782,0.83886255924170616,0.84745762711864403,0.85333333333333339,0.8046875,0.83495145631067957],"type":"bar","name":"ANDROID","marker":{"color":"rgba(102,194,165,1)","line":{"color":"rgba(102,194,165,1)"}},"textfont":{"color":"rgba(102,194,165,1)"},"error_y":{"color":"rgba(102,194,165,1)"},"error_x":{"color":"rgba(102,194,165,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[2,3,4,5,6,7,8,9,10],"y":[0.5934640522875817,0.71145374449339205,0.8173374613003096,0.8125,0.87412587412587417,0.84533333333333338,0.88643533123028395,0.87900355871886116,0.90688259109311742],"type":"bar","name":"IOS","marker":{"color":"rgba(141,160,203,1)","line":{"color":"rgba(141,160,203,1)"}},"textfont":{"color":"rgba(141,160,203,1)"},"error_y":{"color":"rgba(141,160,203,1)"},"error_x":{"color":"rgba(141,160,203,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```


### level 5 (N)

Постройте график накопительной конверсии в когорте июньских пользователей с разбивкой по источнику (media_source) пользователей.

Для этого надо сначала посчитать, в какой день от инсталла пользователь сделал первый платеж (lifetime).
Потом посчитать, сколько пользователей сделало первый платеж в 0-30 дни от инсталла (new payers).
Посчитать накопительную сумму по количеству пользователей (cumulative new payers).
Посчитать отношение cumulative new payers / total users
Нарисовать график.



``` r
# Фильтруем инсталлы до июля
installs_june <- installs %>%
  filter(dt < as.Date("2022-07-01"))

# Первый платёж каждого пользователя
first_payment <- payments %>%
  group_by(user_pseudo_id) %>%
  summarise(pay_dt_first = min(pay_dt), .groups = "drop")

# оставляем только июньских пользователей, вычисляем лайфтайм
first_payment <- first_payment%>%
  inner_join(installs_june %>% select(user_pseudo_id, dt, media_source), by = "user_pseudo_id") %>%
  mutate(lifetime = as.integer(difftime(pay_dt_first, dt, units = "days"))) %>%
  filter(lifetime %in% 0:7)

# Агрегация по источникам и лайфтаймам
first_payment_stat <- first_payment %>%
  group_by(media_source, lifetime) %>%
  summarise(new_payers = n_distinct(user_pseudo_id), .groups = "drop") %>%
  arrange(media_source, lifetime) %>%
  group_by(media_source) %>%
  mutate(new_payers_cum = cumsum(new_payers)) %>%
  ungroup()

# Кол-во инсталлов по источникам
installs_june_stat <- installs_june %>%
  group_by(media_source) %>%
  summarise(total_installs = n_distinct(user_pseudo_id), .groups = "drop")

# Объединение и расчет кумулятивной конверсии
first_payment_stat <- installs_june_stat %>%
  left_join(first_payment_stat, by = "media_source") %>%
  mutate(conversion_cum = new_payers_cum / total_installs)

# Визуализация
plot_ly(
  data = first_payment_stat,
  x = ~lifetime, y = ~conversion_cum, color = ~media_source,
  type = 'scatter', mode = 'lines'
) %>%
  layout(
    title = "Конверсия в платящих по источникам трафика",
    yaxis = list(rangemode = 'tozero')
  )
```

```{=html}
<div class="plotly html-widget html-fill-item" id="htmlwidget-7b3c15f541c8aa927833" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-7b3c15f541c8aa927833">{"x":{"visdat":{"3c1356a2a9178":["function () ","plotlyVisDat"]},"cur_data":"3c1356a2a9178","attrs":{"3c1356a2a9178":{"x":{},"y":{},"mode":"lines","color":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"Конверсия в платящих по источникам трафика","yaxis":{"domain":[0,1],"automargin":true,"rangemode":"tozero","title":"conversion_cum"},"xaxis":{"domain":[0,1],"automargin":true,"title":"lifetime"},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false},"data":[{"x":[0,1,2,3,5,7],"y":[0.013878180416345412,0.022359290670778721,0.024672320740169621,0.030069390902081727,0.030840400925212029,0.031611410948342328],"mode":"lines","type":"scatter","name":"Facebook Ads","marker":{"color":"rgba(252,141,98,1)","line":{"color":"rgba(252,141,98,1)"}},"textfont":{"color":"rgba(252,141,98,1)"},"error_y":{"color":"rgba(252,141,98,1)"},"error_x":{"color":"rgba(252,141,98,1)"},"line":{"color":"rgba(252,141,98,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[0,1,2,3,4,5,6,7],"y":[0.0095876232499863805,0.015144086724410307,0.018167456556082148,0.020046848613607889,0.021272539085907285,0.022770605218717654,0.023778395162608268,0.024350384049681321],"mode":"lines","type":"scatter","name":"applovin_int","marker":{"color":"rgba(102,194,165,1)","line":{"color":"rgba(102,194,165,1)"}},"textfont":{"color":"rgba(102,194,165,1)"},"error_y":{"color":"rgba(102,194,165,1)"},"error_x":{"color":"rgba(102,194,165,1)"},"line":{"color":"rgba(102,194,165,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[0,1,2,3,4,5,6,7],"y":[0.0075962405046993689,0.011072486159392301,0.012874983906270118,0.014162482296897129,0.014677481653147934,0.01532123084846144,0.015707480365649541,0.016093729882837648],"mode":"lines","type":"scatter","name":"googleadwords_int","marker":{"color":"rgba(141,160,203,1)","line":{"color":"rgba(141,160,203,1)"}},"textfont":{"color":"rgba(141,160,203,1)"},"error_y":{"color":"rgba(141,160,203,1)"},"error_x":{"color":"rgba(141,160,203,1)"},"line":{"color":"rgba(141,160,203,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[0,1,2,3,4,5,6,7],"y":[0.017178628621342264,0.02838841170476052,0.032173533265395252,0.035521910030572139,0.039015868394234966,0.039889357985150677,0.040908429174552334,0.042946571553355656],"mode":"lines","type":"scatter","name":"other","marker":{"color":"rgba(231,138,195,1)","line":{"color":"rgba(231,138,195,1)"}},"textfont":{"color":"rgba(231,138,195,1)"},"error_y":{"color":"rgba(231,138,195,1)"},"error_x":{"color":"rgba(231,138,195,1)"},"line":{"color":"rgba(231,138,195,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[0,1,2,3,4,5,6,7],"y":[0.003647638154295094,0.0060641984315155939,0.0071128944008754334,0.0077968265548057636,0.0081159948933065846,0.0089823089549516694,0.0093014772934524887,0.0094838592011672451],"mode":"lines","type":"scatter","name":"unityads_int","marker":{"color":"rgba(166,216,84,1)","line":{"color":"rgba(166,216,84,1)"}},"textfont":{"color":"rgba(166,216,84,1)"},"error_y":{"color":"rgba(166,216,84,1)"},"error_x":{"color":"rgba(166,216,84,1)"},"line":{"color":"rgba(166,216,84,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[0,1,2,3,4,5,6,7],"y":[0.0050042854378058556,0.0082667477674251439,0.0099532749039232486,0.011059194337692499,0.011971577870552131,0.012524537587436756,0.012883961403411762,0.013547513063673312],"mode":"lines","type":"scatter","name":"NA","marker":{"color":"transparent","line":{"color":"transparent"}},"textfont":{"color":"transparent"},"error_y":{"color":"transparent"},"error_x":{"color":"transparent"},"line":{"color":"transparent"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```



