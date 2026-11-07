# `get_estimates()` works with deterministic run

    Code
      estimates_snapshot <- dplyr::select(FIMS:::estimates_big_unoptimized,
      -estimated, -expected, -uncertainty, -gradient, -likelihood, -log_like_cv)
      estimates_snapshot_lines <- capture.output(print(estimates_snapshot, n = 320,
        width = Inf))
      cat(trimws(estimates_snapshot_lines, which = "right"), sep = "\n")
    Output
      # A tibble: 13,967 x 18
          module_name module_id module_type  label            type   type_id
          <chr>           <int> <chr>        <chr>            <chr>    <int>
        1 Selectivity         1 Logistic     inflection_point vector      NA
        2 Selectivity         1 Logistic     slope            vector      NA
        3 Selectivity         2 Logistic     inflection_point vector      NA
        4 Selectivity         2 Logistic     slope            vector      NA
        5 Fleet               1 <NA>         log_q            vector      NA
        6 Fleet               1 <NA>         log_Fmort        vector      NA
        7 Fleet               1 <NA>         log_Fmort        vector      NA
        8 Fleet               1 <NA>         log_Fmort        vector      NA
        9 Fleet               1 <NA>         log_Fmort        vector      NA
       10 Fleet               1 <NA>         log_Fmort        vector      NA
       11 Fleet               1 <NA>         log_Fmort        vector      NA
       12 Fleet               1 <NA>         log_Fmort        vector      NA
       13 Fleet               1 <NA>         log_Fmort        vector      NA
       14 Fleet               1 <NA>         log_Fmort        vector      NA
       15 Fleet               1 <NA>         log_Fmort        vector      NA
       16 Fleet               1 <NA>         log_Fmort        vector      NA
       17 Fleet               1 <NA>         log_Fmort        vector      NA
       18 Fleet               1 <NA>         log_Fmort        vector      NA
       19 Fleet               1 <NA>         log_Fmort        vector      NA
       20 Fleet               1 <NA>         log_Fmort        vector      NA
       21 Fleet               1 <NA>         log_Fmort        vector      NA
       22 Fleet               1 <NA>         log_Fmort        vector      NA
       23 Fleet               1 <NA>         log_Fmort        vector      NA
       24 Fleet               1 <NA>         log_Fmort        vector      NA
       25 Fleet               1 <NA>         log_Fmort        vector      NA
       26 Fleet               1 <NA>         log_Fmort        vector      NA
       27 Fleet               1 <NA>         log_Fmort        vector      NA
       28 Fleet               1 <NA>         log_Fmort        vector      NA
       29 Fleet               1 <NA>         log_Fmort        vector      NA
       30 Fleet               1 <NA>         log_Fmort        vector      NA
       31 Fleet               1 <NA>         log_Fmort        vector      NA
       32 Fleet               1 <NA>         log_Fmort        vector      NA
       33 Fleet               1 <NA>         log_Fmort        vector      NA
       34 Fleet               1 <NA>         log_Fmort        vector      NA
       35 Fleet               1 <NA>         log_Fmort        vector      NA
       36 Fleet               2 <NA>         log_q            vector      NA
       37 Fleet               2 <NA>         log_Fmort        vector      NA
       38 Fleet               2 <NA>         log_Fmort        vector      NA
       39 Fleet               2 <NA>         log_Fmort        vector      NA
       40 Fleet               2 <NA>         log_Fmort        vector      NA
       41 Fleet               2 <NA>         log_Fmort        vector      NA
       42 Fleet               2 <NA>         log_Fmort        vector      NA
       43 Fleet               2 <NA>         log_Fmort        vector      NA
       44 Fleet               2 <NA>         log_Fmort        vector      NA
       45 Fleet               2 <NA>         log_Fmort        vector      NA
       46 Fleet               2 <NA>         log_Fmort        vector      NA
       47 Fleet               2 <NA>         log_Fmort        vector      NA
       48 Fleet               2 <NA>         log_Fmort        vector      NA
       49 Fleet               2 <NA>         log_Fmort        vector      NA
       50 Fleet               2 <NA>         log_Fmort        vector      NA
       51 Fleet               2 <NA>         log_Fmort        vector      NA
       52 Fleet               2 <NA>         log_Fmort        vector      NA
       53 Fleet               2 <NA>         log_Fmort        vector      NA
       54 Fleet               2 <NA>         log_Fmort        vector      NA
       55 Fleet               2 <NA>         log_Fmort        vector      NA
       56 Fleet               2 <NA>         log_Fmort        vector      NA
       57 Fleet               2 <NA>         log_Fmort        vector      NA
       58 Fleet               2 <NA>         log_Fmort        vector      NA
       59 Fleet               2 <NA>         log_Fmort        vector      NA
       60 Fleet               2 <NA>         log_Fmort        vector      NA
       61 Fleet               2 <NA>         log_Fmort        vector      NA
       62 Fleet               2 <NA>         log_Fmort        vector      NA
       63 Fleet               2 <NA>         log_Fmort        vector      NA
       64 Fleet               2 <NA>         log_Fmort        vector      NA
       65 Fleet               2 <NA>         log_Fmort        vector      NA
       66 Fleet               2 <NA>         log_Fmort        vector      NA
       67 Recruitment         1 BevertonHolt log_rzero        vector      NA
       68 Recruitment         1 BevertonHolt logit_steep      vector      NA
       69 Recruitment         1 BevertonHolt log_devs         vector      NA
       70 Recruitment         1 BevertonHolt log_devs         vector      NA
       71 Recruitment         1 BevertonHolt log_devs         vector      NA
       72 Recruitment         1 BevertonHolt log_devs         vector      NA
       73 Recruitment         1 BevertonHolt log_devs         vector      NA
       74 Recruitment         1 BevertonHolt log_devs         vector      NA
       75 Recruitment         1 BevertonHolt log_devs         vector      NA
       76 Recruitment         1 BevertonHolt log_devs         vector      NA
       77 Recruitment         1 BevertonHolt log_devs         vector      NA
       78 Recruitment         1 BevertonHolt log_devs         vector      NA
       79 Recruitment         1 BevertonHolt log_devs         vector      NA
       80 Recruitment         1 BevertonHolt log_devs         vector      NA
       81 Recruitment         1 BevertonHolt log_devs         vector      NA
       82 Recruitment         1 BevertonHolt log_devs         vector      NA
       83 Recruitment         1 BevertonHolt log_devs         vector      NA
       84 Recruitment         1 BevertonHolt log_devs         vector      NA
       85 Recruitment         1 BevertonHolt log_devs         vector      NA
       86 Recruitment         1 BevertonHolt log_devs         vector      NA
       87 Recruitment         1 BevertonHolt log_devs         vector      NA
       88 Recruitment         1 BevertonHolt log_devs         vector      NA
       89 Recruitment         1 BevertonHolt log_devs         vector      NA
       90 Recruitment         1 BevertonHolt log_devs         vector      NA
       91 Recruitment         1 BevertonHolt log_devs         vector      NA
       92 Recruitment         1 BevertonHolt log_devs         vector      NA
       93 Recruitment         1 BevertonHolt log_devs         vector      NA
       94 Recruitment         1 BevertonHolt log_devs         vector      NA
       95 Recruitment         1 BevertonHolt log_devs         vector      NA
       96 Recruitment         1 BevertonHolt log_devs         vector      NA
       97 Recruitment         1 BevertonHolt log_devs         vector      NA
       98 Recruitment         1 BevertonHolt log_sd           vector      NA
       99 Maturity            1 Logistic     inflection_point vector      NA
      100 Maturity            1 Logistic     slope            vector      NA
      101 Population         NA <NA>         log_M            vector      NA
      102 Population         NA <NA>         log_M            vector      NA
      103 Population         NA <NA>         log_M            vector      NA
      104 Population         NA <NA>         log_M            vector      NA
      105 Population         NA <NA>         log_M            vector      NA
      106 Population         NA <NA>         log_M            vector      NA
      107 Population         NA <NA>         log_M            vector      NA
      108 Population         NA <NA>         log_M            vector      NA
      109 Population         NA <NA>         log_M            vector      NA
      110 Population         NA <NA>         log_M            vector      NA
      111 Population         NA <NA>         log_M            vector      NA
      112 Population         NA <NA>         log_M            vector      NA
      113 Population         NA <NA>         log_M            vector      NA
      114 Population         NA <NA>         log_M            vector      NA
      115 Population         NA <NA>         log_M            vector      NA
      116 Population         NA <NA>         log_M            vector      NA
      117 Population         NA <NA>         log_M            vector      NA
      118 Population         NA <NA>         log_M            vector      NA
      119 Population         NA <NA>         log_M            vector      NA
      120 Population         NA <NA>         log_M            vector      NA
      121 Population         NA <NA>         log_M            vector      NA
      122 Population         NA <NA>         log_M            vector      NA
      123 Population         NA <NA>         log_M            vector      NA
      124 Population         NA <NA>         log_M            vector      NA
      125 Population         NA <NA>         log_M            vector      NA
      126 Population         NA <NA>         log_M            vector      NA
      127 Population         NA <NA>         log_M            vector      NA
      128 Population         NA <NA>         log_M            vector      NA
      129 Population         NA <NA>         log_M            vector      NA
      130 Population         NA <NA>         log_M            vector      NA
      131 Population         NA <NA>         log_M            vector      NA
      132 Population         NA <NA>         log_M            vector      NA
      133 Population         NA <NA>         log_M            vector      NA
      134 Population         NA <NA>         log_M            vector      NA
      135 Population         NA <NA>         log_M            vector      NA
      136 Population         NA <NA>         log_M            vector      NA
      137 Population         NA <NA>         log_M            vector      NA
      138 Population         NA <NA>         log_M            vector      NA
      139 Population         NA <NA>         log_M            vector      NA
      140 Population         NA <NA>         log_M            vector      NA
      141 Population         NA <NA>         log_M            vector      NA
      142 Population         NA <NA>         log_M            vector      NA
      143 Population         NA <NA>         log_M            vector      NA
      144 Population         NA <NA>         log_M            vector      NA
      145 Population         NA <NA>         log_M            vector      NA
      146 Population         NA <NA>         log_M            vector      NA
      147 Population         NA <NA>         log_M            vector      NA
      148 Population         NA <NA>         log_M            vector      NA
      149 Population         NA <NA>         log_M            vector      NA
      150 Population         NA <NA>         log_M            vector      NA
      151 Population         NA <NA>         log_M            vector      NA
      152 Population         NA <NA>         log_M            vector      NA
      153 Population         NA <NA>         log_M            vector      NA
      154 Population         NA <NA>         log_M            vector      NA
      155 Population         NA <NA>         log_M            vector      NA
      156 Population         NA <NA>         log_M            vector      NA
      157 Population         NA <NA>         log_M            vector      NA
      158 Population         NA <NA>         log_M            vector      NA
      159 Population         NA <NA>         log_M            vector      NA
      160 Population         NA <NA>         log_M            vector      NA
      161 Population         NA <NA>         log_M            vector      NA
      162 Population         NA <NA>         log_M            vector      NA
      163 Population         NA <NA>         log_M            vector      NA
      164 Population         NA <NA>         log_M            vector      NA
      165 Population         NA <NA>         log_M            vector      NA
      166 Population         NA <NA>         log_M            vector      NA
      167 Population         NA <NA>         log_M            vector      NA
      168 Population         NA <NA>         log_M            vector      NA
      169 Population         NA <NA>         log_M            vector      NA
      170 Population         NA <NA>         log_M            vector      NA
      171 Population         NA <NA>         log_M            vector      NA
      172 Population         NA <NA>         log_M            vector      NA
      173 Population         NA <NA>         log_M            vector      NA
      174 Population         NA <NA>         log_M            vector      NA
      175 Population         NA <NA>         log_M            vector      NA
      176 Population         NA <NA>         log_M            vector      NA
      177 Population         NA <NA>         log_M            vector      NA
      178 Population         NA <NA>         log_M            vector      NA
      179 Population         NA <NA>         log_M            vector      NA
      180 Population         NA <NA>         log_M            vector      NA
      181 Population         NA <NA>         log_M            vector      NA
      182 Population         NA <NA>         log_M            vector      NA
      183 Population         NA <NA>         log_M            vector      NA
      184 Population         NA <NA>         log_M            vector      NA
      185 Population         NA <NA>         log_M            vector      NA
      186 Population         NA <NA>         log_M            vector      NA
      187 Population         NA <NA>         log_M            vector      NA
      188 Population         NA <NA>         log_M            vector      NA
      189 Population         NA <NA>         log_M            vector      NA
      190 Population         NA <NA>         log_M            vector      NA
      191 Population         NA <NA>         log_M            vector      NA
      192 Population         NA <NA>         log_M            vector      NA
      193 Population         NA <NA>         log_M            vector      NA
      194 Population         NA <NA>         log_M            vector      NA
      195 Population         NA <NA>         log_M            vector      NA
      196 Population         NA <NA>         log_M            vector      NA
      197 Population         NA <NA>         log_M            vector      NA
      198 Population         NA <NA>         log_M            vector      NA
      199 Population         NA <NA>         log_M            vector      NA
      200 Population         NA <NA>         log_M            vector      NA
      201 Population         NA <NA>         log_M            vector      NA
      202 Population         NA <NA>         log_M            vector      NA
      203 Population         NA <NA>         log_M            vector      NA
      204 Population         NA <NA>         log_M            vector      NA
      205 Population         NA <NA>         log_M            vector      NA
      206 Population         NA <NA>         log_M            vector      NA
      207 Population         NA <NA>         log_M            vector      NA
      208 Population         NA <NA>         log_M            vector      NA
      209 Population         NA <NA>         log_M            vector      NA
      210 Population         NA <NA>         log_M            vector      NA
      211 Population         NA <NA>         log_M            vector      NA
      212 Population         NA <NA>         log_M            vector      NA
      213 Population         NA <NA>         log_M            vector      NA
      214 Population         NA <NA>         log_M            vector      NA
      215 Population         NA <NA>         log_M            vector      NA
      216 Population         NA <NA>         log_M            vector      NA
      217 Population         NA <NA>         log_M            vector      NA
      218 Population         NA <NA>         log_M            vector      NA
      219 Population         NA <NA>         log_M            vector      NA
      220 Population         NA <NA>         log_M            vector      NA
      221 Population         NA <NA>         log_M            vector      NA
      222 Population         NA <NA>         log_M            vector      NA
      223 Population         NA <NA>         log_M            vector      NA
      224 Population         NA <NA>         log_M            vector      NA
      225 Population         NA <NA>         log_M            vector      NA
      226 Population         NA <NA>         log_M            vector      NA
      227 Population         NA <NA>         log_M            vector      NA
      228 Population         NA <NA>         log_M            vector      NA
      229 Population         NA <NA>         log_M            vector      NA
      230 Population         NA <NA>         log_M            vector      NA
      231 Population         NA <NA>         log_M            vector      NA
      232 Population         NA <NA>         log_M            vector      NA
      233 Population         NA <NA>         log_M            vector      NA
      234 Population         NA <NA>         log_M            vector      NA
      235 Population         NA <NA>         log_M            vector      NA
      236 Population         NA <NA>         log_M            vector      NA
      237 Population         NA <NA>         log_M            vector      NA
      238 Population         NA <NA>         log_M            vector      NA
      239 Population         NA <NA>         log_M            vector      NA
      240 Population         NA <NA>         log_M            vector      NA
      241 Population         NA <NA>         log_M            vector      NA
      242 Population         NA <NA>         log_M            vector      NA
      243 Population         NA <NA>         log_M            vector      NA
      244 Population         NA <NA>         log_M            vector      NA
      245 Population         NA <NA>         log_M            vector      NA
      246 Population         NA <NA>         log_M            vector      NA
      247 Population         NA <NA>         log_M            vector      NA
      248 Population         NA <NA>         log_M            vector      NA
      249 Population         NA <NA>         log_M            vector      NA
      250 Population         NA <NA>         log_M            vector      NA
      251 Population         NA <NA>         log_M            vector      NA
      252 Population         NA <NA>         log_M            vector      NA
      253 Population         NA <NA>         log_M            vector      NA
      254 Population         NA <NA>         log_M            vector      NA
      255 Population         NA <NA>         log_M            vector      NA
      256 Population         NA <NA>         log_M            vector      NA
      257 Population         NA <NA>         log_M            vector      NA
      258 Population         NA <NA>         log_M            vector      NA
      259 Population         NA <NA>         log_M            vector      NA
      260 Population         NA <NA>         log_M            vector      NA
      261 Population         NA <NA>         log_M            vector      NA
      262 Population         NA <NA>         log_M            vector      NA
      263 Population         NA <NA>         log_M            vector      NA
      264 Population         NA <NA>         log_M            vector      NA
      265 Population         NA <NA>         log_M            vector      NA
      266 Population         NA <NA>         log_M            vector      NA
      267 Population         NA <NA>         log_M            vector      NA
      268 Population         NA <NA>         log_M            vector      NA
      269 Population         NA <NA>         log_M            vector      NA
      270 Population         NA <NA>         log_M            vector      NA
      271 Population         NA <NA>         log_M            vector      NA
      272 Population         NA <NA>         log_M            vector      NA
      273 Population         NA <NA>         log_M            vector      NA
      274 Population         NA <NA>         log_M            vector      NA
      275 Population         NA <NA>         log_M            vector      NA
      276 Population         NA <NA>         log_M            vector      NA
      277 Population         NA <NA>         log_M            vector      NA
      278 Population         NA <NA>         log_M            vector      NA
      279 Population         NA <NA>         log_M            vector      NA
      280 Population         NA <NA>         log_M            vector      NA
      281 Population         NA <NA>         log_M            vector      NA
      282 Population         NA <NA>         log_M            vector      NA
      283 Population         NA <NA>         log_M            vector      NA
      284 Population         NA <NA>         log_M            vector      NA
      285 Population         NA <NA>         log_M            vector      NA
      286 Population         NA <NA>         log_M            vector      NA
      287 Population         NA <NA>         log_M            vector      NA
      288 Population         NA <NA>         log_M            vector      NA
      289 Population         NA <NA>         log_M            vector      NA
      290 Population         NA <NA>         log_M            vector      NA
      291 Population         NA <NA>         log_M            vector      NA
      292 Population         NA <NA>         log_M            vector      NA
      293 Population         NA <NA>         log_M            vector      NA
      294 Population         NA <NA>         log_M            vector      NA
      295 Population         NA <NA>         log_M            vector      NA
      296 Population         NA <NA>         log_M            vector      NA
      297 Population         NA <NA>         log_M            vector      NA
      298 Population         NA <NA>         log_M            vector      NA
      299 Population         NA <NA>         log_M            vector      NA
      300 Population         NA <NA>         log_M            vector      NA
      301 Population         NA <NA>         log_M            vector      NA
      302 Population         NA <NA>         log_M            vector      NA
      303 Population         NA <NA>         log_M            vector      NA
      304 Population         NA <NA>         log_M            vector      NA
      305 Population         NA <NA>         log_M            vector      NA
      306 Population         NA <NA>         log_M            vector      NA
      307 Population         NA <NA>         log_M            vector      NA
      308 Population         NA <NA>         log_M            vector      NA
      309 Population         NA <NA>         log_M            vector      NA
      310 Population         NA <NA>         log_M            vector      NA
      311 Population         NA <NA>         log_M            vector      NA
      312 Population         NA <NA>         log_M            vector      NA
      313 Population         NA <NA>         log_M            vector      NA
      314 Population         NA <NA>         log_M            vector      NA
      315 Population         NA <NA>         log_M            vector      NA
      316 Population         NA <NA>         log_M            vector      NA
      317 Population         NA <NA>         log_M            vector      NA
      318 Population         NA <NA>         log_M            vector      NA
      319 Population         NA <NA>         log_M            vector      NA
      320 Population         NA <NA>         log_M            vector      NA
          parameter_id fleet   year_i   age length     input observed estimation_type
                 <int> <chr>    <dbl> <dbl>  <dbl>     <dbl>    <dbl> <chr>
        1            1 fleet1      NA    NA     NA    2            NA fixed_effects
        2            2 fleet1      NA    NA     NA    1            NA fixed_effects
        3            3 survey1     NA    NA     NA    1.5          NA fixed_effects
        4            4 survey1     NA    NA     NA    2            NA fixed_effects
        5            5 fleet1      NA    NA     NA    0            NA constant
        6            6 fleet1       1    NA     NA   -4.66         NA fixed_effects
        7            7 fleet1       2    NA     NA   -3.60         NA fixed_effects
        8            8 fleet1       3    NA     NA   -3.10         NA fixed_effects
        9            9 fleet1       4    NA     NA   -2.80         NA fixed_effects
       10           10 fleet1       5    NA     NA   -3.02         NA fixed_effects
       11           11 fleet1       6    NA     NA   -2.44         NA fixed_effects
       12           12 fleet1       7    NA     NA   -2.43         NA fixed_effects
       13           13 fleet1       8    NA     NA   -1.68         NA fixed_effects
       14           14 fleet1       9    NA     NA   -2.22         NA fixed_effects
       15           15 fleet1      10    NA     NA   -2.02         NA fixed_effects
       16           16 fleet1      11    NA     NA   -1.89         NA fixed_effects
       17           17 fleet1      12    NA     NA   -1.82         NA fixed_effects
       18           18 fleet1      13    NA     NA   -2.15         NA fixed_effects
       19           19 fleet1      14    NA     NA   -1.78         NA fixed_effects
       20           20 fleet1      15    NA     NA   -1.71         NA fixed_effects
       21           21 fleet1      16    NA     NA   -1.82         NA fixed_effects
       22           22 fleet1      17    NA     NA   -1.16         NA fixed_effects
       23           23 fleet1      18    NA     NA   -1.36         NA fixed_effects
       24           24 fleet1      19    NA     NA   -1.37         NA fixed_effects
       25           25 fleet1      20    NA     NA   -1.38         NA fixed_effects
       26           26 fleet1      21    NA     NA   -1.05         NA fixed_effects
       27           27 fleet1      22    NA     NA   -1.37         NA fixed_effects
       28           28 fleet1      23    NA     NA   -0.871        NA fixed_effects
       29           29 fleet1      24    NA     NA   -1.06         NA fixed_effects
       30           30 fleet1      25    NA     NA   -1.07         NA fixed_effects
       31           31 fleet1      26    NA     NA   -1.16         NA fixed_effects
       32           32 fleet1      27    NA     NA   -1.18         NA fixed_effects
       33           33 fleet1      28    NA     NA   -0.840        NA fixed_effects
       34           34 fleet1      29    NA     NA   -1.11         NA fixed_effects
       35           35 fleet1      30    NA     NA   -0.694        NA fixed_effects
       36           36 survey1     NA    NA     NA  -14.9          NA fixed_effects
       37           37 survey1      1    NA     NA -200            NA constant
       38           38 survey1      2    NA     NA -200            NA constant
       39           39 survey1      3    NA     NA -200            NA constant
       40           40 survey1      4    NA     NA -200            NA constant
       41           41 survey1      5    NA     NA -200            NA constant
       42           42 survey1      6    NA     NA -200            NA constant
       43           43 survey1      7    NA     NA -200            NA constant
       44           44 survey1      8    NA     NA -200            NA constant
       45           45 survey1      9    NA     NA -200            NA constant
       46           46 survey1     10    NA     NA -200            NA constant
       47           47 survey1     11    NA     NA -200            NA constant
       48           48 survey1     12    NA     NA -200            NA constant
       49           49 survey1     13    NA     NA -200            NA constant
       50           50 survey1     14    NA     NA -200            NA constant
       51           51 survey1     15    NA     NA -200            NA constant
       52           52 survey1     16    NA     NA -200            NA constant
       53           53 survey1     17    NA     NA -200            NA constant
       54           54 survey1     18    NA     NA -200            NA constant
       55           55 survey1     19    NA     NA -200            NA constant
       56           56 survey1     20    NA     NA -200            NA constant
       57           57 survey1     21    NA     NA -200            NA constant
       58           58 survey1     22    NA     NA -200            NA constant
       59           59 survey1     23    NA     NA -200            NA constant
       60           60 survey1     24    NA     NA -200            NA constant
       61           61 survey1     25    NA     NA -200            NA constant
       62           62 survey1     26    NA     NA -200            NA constant
       63           63 survey1     27    NA     NA -200            NA constant
       64           64 survey1     28    NA     NA -200            NA constant
       65           65 survey1     29    NA     NA -200            NA constant
       66           66 survey1     30    NA     NA -200            NA constant
       67           67 <NA>        NA    NA     NA   13.8          NA fixed_effects
       68           68 <NA>        NA    NA     NA    0.788        NA constant
       69           69 <NA>         2    NA     NA    0.438        NA random_effects
       70           70 <NA>         3    NA     NA   -0.133        NA random_effects
       71           71 <NA>         4    NA     NA   -0.433        NA random_effects
       72           72 <NA>         5    NA     NA    0.649        NA random_effects
       73           73 <NA>         6    NA     NA    0.506        NA random_effects
       74           74 <NA>         7    NA     NA   -0.0696       NA random_effects
       75           75 <NA>         8    NA     NA    0.302        NA random_effects
       76           76 <NA>         9    NA     NA   -0.0826       NA random_effects
       77           77 <NA>        10    NA     NA    0.207        NA random_effects
       78           78 <NA>        11    NA     NA    0.153        NA random_effects
       79           79 <NA>        12    NA     NA   -0.217        NA random_effects
       80           80 <NA>        13    NA     NA   -0.133        NA random_effects
       81           81 <NA>        14    NA     NA    0.112        NA random_effects
       82           82 <NA>        15    NA     NA   -0.107        NA random_effects
       83           83 <NA>        16    NA     NA    0.269        NA random_effects
       84           84 <NA>        17    NA     NA    0.241        NA random_effects
       85           85 <NA>        18    NA     NA   -0.545        NA random_effects
       86           86 <NA>        19    NA     NA   -0.237        NA random_effects
       87           87 <NA>        20    NA     NA   -0.585        NA random_effects
       88           88 <NA>        21    NA     NA    0.301        NA random_effects
       89           89 <NA>        22    NA     NA    0.219        NA random_effects
       90           90 <NA>        23    NA     NA   -0.223        NA random_effects
       91           91 <NA>        24    NA     NA   -0.514        NA random_effects
       92           92 <NA>        25    NA     NA    0.157        NA random_effects
       93           93 <NA>        26    NA     NA   -0.540        NA random_effects
       94           94 <NA>        27    NA     NA   -0.196        NA random_effects
       95           95 <NA>        28    NA     NA    0.201        NA random_effects
       96           96 <NA>        29    NA     NA    0.372        NA random_effects
       97           97 <NA>        30    NA     NA   -0.0716       NA random_effects
       98           98 <NA>        NA    NA     NA   -0.916        NA fixed_effects
       99           99 <NA>        NA    NA     NA    2.25         NA constant
      100          100 <NA>        NA    NA     NA    3            NA constant
      101          101 <NA>         1     1     NA   -1.61         NA constant
      102          102 <NA>         1     2     NA   -1.61         NA constant
      103          103 <NA>         1     3     NA   -1.61         NA constant
      104          104 <NA>         1     4     NA   -1.61         NA constant
      105          105 <NA>         1     5     NA   -1.61         NA constant
      106          106 <NA>         1     6     NA   -1.61         NA constant
      107          107 <NA>         1     7     NA   -1.61         NA constant
      108          108 <NA>         1     8     NA   -1.61         NA constant
      109          109 <NA>         1     9     NA   -1.61         NA constant
      110          110 <NA>         1    10     NA   -1.61         NA constant
      111          111 <NA>         1    11     NA   -1.61         NA constant
      112          112 <NA>         1    12     NA   -1.61         NA constant
      113          113 <NA>         2     1     NA   -1.61         NA constant
      114          114 <NA>         2     2     NA   -1.61         NA constant
      115          115 <NA>         2     3     NA   -1.61         NA constant
      116          116 <NA>         2     4     NA   -1.61         NA constant
      117          117 <NA>         2     5     NA   -1.61         NA constant
      118          118 <NA>         2     6     NA   -1.61         NA constant
      119          119 <NA>         2     7     NA   -1.61         NA constant
      120          120 <NA>         2     8     NA   -1.61         NA constant
      121          121 <NA>         2     9     NA   -1.61         NA constant
      122          122 <NA>         2    10     NA   -1.61         NA constant
      123          123 <NA>         2    11     NA   -1.61         NA constant
      124          124 <NA>         2    12     NA   -1.61         NA constant
      125          125 <NA>         3     1     NA   -1.61         NA constant
      126          126 <NA>         3     2     NA   -1.61         NA constant
      127          127 <NA>         3     3     NA   -1.61         NA constant
      128          128 <NA>         3     4     NA   -1.61         NA constant
      129          129 <NA>         3     5     NA   -1.61         NA constant
      130          130 <NA>         3     6     NA   -1.61         NA constant
      131          131 <NA>         3     7     NA   -1.61         NA constant
      132          132 <NA>         3     8     NA   -1.61         NA constant
      133          133 <NA>         3     9     NA   -1.61         NA constant
      134          134 <NA>         3    10     NA   -1.61         NA constant
      135          135 <NA>         3    11     NA   -1.61         NA constant
      136          136 <NA>         3    12     NA   -1.61         NA constant
      137          137 <NA>         4     1     NA   -1.61         NA constant
      138          138 <NA>         4     2     NA   -1.61         NA constant
      139          139 <NA>         4     3     NA   -1.61         NA constant
      140          140 <NA>         4     4     NA   -1.61         NA constant
      141          141 <NA>         4     5     NA   -1.61         NA constant
      142          142 <NA>         4     6     NA   -1.61         NA constant
      143          143 <NA>         4     7     NA   -1.61         NA constant
      144          144 <NA>         4     8     NA   -1.61         NA constant
      145          145 <NA>         4     9     NA   -1.61         NA constant
      146          146 <NA>         4    10     NA   -1.61         NA constant
      147          147 <NA>         4    11     NA   -1.61         NA constant
      148          148 <NA>         4    12     NA   -1.61         NA constant
      149          149 <NA>         5     1     NA   -1.61         NA constant
      150          150 <NA>         5     2     NA   -1.61         NA constant
      151          151 <NA>         5     3     NA   -1.61         NA constant
      152          152 <NA>         5     4     NA   -1.61         NA constant
      153          153 <NA>         5     5     NA   -1.61         NA constant
      154          154 <NA>         5     6     NA   -1.61         NA constant
      155          155 <NA>         5     7     NA   -1.61         NA constant
      156          156 <NA>         5     8     NA   -1.61         NA constant
      157          157 <NA>         5     9     NA   -1.61         NA constant
      158          158 <NA>         5    10     NA   -1.61         NA constant
      159          159 <NA>         5    11     NA   -1.61         NA constant
      160          160 <NA>         5    12     NA   -1.61         NA constant
      161          161 <NA>         6     1     NA   -1.61         NA constant
      162          162 <NA>         6     2     NA   -1.61         NA constant
      163          163 <NA>         6     3     NA   -1.61         NA constant
      164          164 <NA>         6     4     NA   -1.61         NA constant
      165          165 <NA>         6     5     NA   -1.61         NA constant
      166          166 <NA>         6     6     NA   -1.61         NA constant
      167          167 <NA>         6     7     NA   -1.61         NA constant
      168          168 <NA>         6     8     NA   -1.61         NA constant
      169          169 <NA>         6     9     NA   -1.61         NA constant
      170          170 <NA>         6    10     NA   -1.61         NA constant
      171          171 <NA>         6    11     NA   -1.61         NA constant
      172          172 <NA>         6    12     NA   -1.61         NA constant
      173          173 <NA>         7     1     NA   -1.61         NA constant
      174          174 <NA>         7     2     NA   -1.61         NA constant
      175          175 <NA>         7     3     NA   -1.61         NA constant
      176          176 <NA>         7     4     NA   -1.61         NA constant
      177          177 <NA>         7     5     NA   -1.61         NA constant
      178          178 <NA>         7     6     NA   -1.61         NA constant
      179          179 <NA>         7     7     NA   -1.61         NA constant
      180          180 <NA>         7     8     NA   -1.61         NA constant
      181          181 <NA>         7     9     NA   -1.61         NA constant
      182          182 <NA>         7    10     NA   -1.61         NA constant
      183          183 <NA>         7    11     NA   -1.61         NA constant
      184          184 <NA>         7    12     NA   -1.61         NA constant
      185          185 <NA>         8     1     NA   -1.61         NA constant
      186          186 <NA>         8     2     NA   -1.61         NA constant
      187          187 <NA>         8     3     NA   -1.61         NA constant
      188          188 <NA>         8     4     NA   -1.61         NA constant
      189          189 <NA>         8     5     NA   -1.61         NA constant
      190          190 <NA>         8     6     NA   -1.61         NA constant
      191          191 <NA>         8     7     NA   -1.61         NA constant
      192          192 <NA>         8     8     NA   -1.61         NA constant
      193          193 <NA>         8     9     NA   -1.61         NA constant
      194          194 <NA>         8    10     NA   -1.61         NA constant
      195          195 <NA>         8    11     NA   -1.61         NA constant
      196          196 <NA>         8    12     NA   -1.61         NA constant
      197          197 <NA>         9     1     NA   -1.61         NA constant
      198          198 <NA>         9     2     NA   -1.61         NA constant
      199          199 <NA>         9     3     NA   -1.61         NA constant
      200          200 <NA>         9     4     NA   -1.61         NA constant
      201          201 <NA>         9     5     NA   -1.61         NA constant
      202          202 <NA>         9     6     NA   -1.61         NA constant
      203          203 <NA>         9     7     NA   -1.61         NA constant
      204          204 <NA>         9     8     NA   -1.61         NA constant
      205          205 <NA>         9     9     NA   -1.61         NA constant
      206          206 <NA>         9    10     NA   -1.61         NA constant
      207          207 <NA>         9    11     NA   -1.61         NA constant
      208          208 <NA>         9    12     NA   -1.61         NA constant
      209          209 <NA>        10     1     NA   -1.61         NA constant
      210          210 <NA>        10     2     NA   -1.61         NA constant
      211          211 <NA>        10     3     NA   -1.61         NA constant
      212          212 <NA>        10     4     NA   -1.61         NA constant
      213          213 <NA>        10     5     NA   -1.61         NA constant
      214          214 <NA>        10     6     NA   -1.61         NA constant
      215          215 <NA>        10     7     NA   -1.61         NA constant
      216          216 <NA>        10     8     NA   -1.61         NA constant
      217          217 <NA>        10     9     NA   -1.61         NA constant
      218          218 <NA>        10    10     NA   -1.61         NA constant
      219          219 <NA>        10    11     NA   -1.61         NA constant
      220          220 <NA>        10    12     NA   -1.61         NA constant
      221          221 <NA>        11     1     NA   -1.61         NA constant
      222          222 <NA>        11     2     NA   -1.61         NA constant
      223          223 <NA>        11     3     NA   -1.61         NA constant
      224          224 <NA>        11     4     NA   -1.61         NA constant
      225          225 <NA>        11     5     NA   -1.61         NA constant
      226          226 <NA>        11     6     NA   -1.61         NA constant
      227          227 <NA>        11     7     NA   -1.61         NA constant
      228          228 <NA>        11     8     NA   -1.61         NA constant
      229          229 <NA>        11     9     NA   -1.61         NA constant
      230          230 <NA>        11    10     NA   -1.61         NA constant
      231          231 <NA>        11    11     NA   -1.61         NA constant
      232          232 <NA>        11    12     NA   -1.61         NA constant
      233          233 <NA>        12     1     NA   -1.61         NA constant
      234          234 <NA>        12     2     NA   -1.61         NA constant
      235          235 <NA>        12     3     NA   -1.61         NA constant
      236          236 <NA>        12     4     NA   -1.61         NA constant
      237          237 <NA>        12     5     NA   -1.61         NA constant
      238          238 <NA>        12     6     NA   -1.61         NA constant
      239          239 <NA>        12     7     NA   -1.61         NA constant
      240          240 <NA>        12     8     NA   -1.61         NA constant
      241          241 <NA>        12     9     NA   -1.61         NA constant
      242          242 <NA>        12    10     NA   -1.61         NA constant
      243          243 <NA>        12    11     NA   -1.61         NA constant
      244          244 <NA>        12    12     NA   -1.61         NA constant
      245          245 <NA>        13     1     NA   -1.61         NA constant
      246          246 <NA>        13     2     NA   -1.61         NA constant
      247          247 <NA>        13     3     NA   -1.61         NA constant
      248          248 <NA>        13     4     NA   -1.61         NA constant
      249          249 <NA>        13     5     NA   -1.61         NA constant
      250          250 <NA>        13     6     NA   -1.61         NA constant
      251          251 <NA>        13     7     NA   -1.61         NA constant
      252          252 <NA>        13     8     NA   -1.61         NA constant
      253          253 <NA>        13     9     NA   -1.61         NA constant
      254          254 <NA>        13    10     NA   -1.61         NA constant
      255          255 <NA>        13    11     NA   -1.61         NA constant
      256          256 <NA>        13    12     NA   -1.61         NA constant
      257          257 <NA>        14     1     NA   -1.61         NA constant
      258          258 <NA>        14     2     NA   -1.61         NA constant
      259          259 <NA>        14     3     NA   -1.61         NA constant
      260          260 <NA>        14     4     NA   -1.61         NA constant
      261          261 <NA>        14     5     NA   -1.61         NA constant
      262          262 <NA>        14     6     NA   -1.61         NA constant
      263          263 <NA>        14     7     NA   -1.61         NA constant
      264          264 <NA>        14     8     NA   -1.61         NA constant
      265          265 <NA>        14     9     NA   -1.61         NA constant
      266          266 <NA>        14    10     NA   -1.61         NA constant
      267          267 <NA>        14    11     NA   -1.61         NA constant
      268          268 <NA>        14    12     NA   -1.61         NA constant
      269          269 <NA>        15     1     NA   -1.61         NA constant
      270          270 <NA>        15     2     NA   -1.61         NA constant
      271          271 <NA>        15     3     NA   -1.61         NA constant
      272          272 <NA>        15     4     NA   -1.61         NA constant
      273          273 <NA>        15     5     NA   -1.61         NA constant
      274          274 <NA>        15     6     NA   -1.61         NA constant
      275          275 <NA>        15     7     NA   -1.61         NA constant
      276          276 <NA>        15     8     NA   -1.61         NA constant
      277          277 <NA>        15     9     NA   -1.61         NA constant
      278          278 <NA>        15    10     NA   -1.61         NA constant
      279          279 <NA>        15    11     NA   -1.61         NA constant
      280          280 <NA>        15    12     NA   -1.61         NA constant
      281          281 <NA>        16     1     NA   -1.61         NA constant
      282          282 <NA>        16     2     NA   -1.61         NA constant
      283          283 <NA>        16     3     NA   -1.61         NA constant
      284          284 <NA>        16     4     NA   -1.61         NA constant
      285          285 <NA>        16     5     NA   -1.61         NA constant
      286          286 <NA>        16     6     NA   -1.61         NA constant
      287          287 <NA>        16     7     NA   -1.61         NA constant
      288          288 <NA>        16     8     NA   -1.61         NA constant
      289          289 <NA>        16     9     NA   -1.61         NA constant
      290          290 <NA>        16    10     NA   -1.61         NA constant
      291          291 <NA>        16    11     NA   -1.61         NA constant
      292          292 <NA>        16    12     NA   -1.61         NA constant
      293          293 <NA>        17     1     NA   -1.61         NA constant
      294          294 <NA>        17     2     NA   -1.61         NA constant
      295          295 <NA>        17     3     NA   -1.61         NA constant
      296          296 <NA>        17     4     NA   -1.61         NA constant
      297          297 <NA>        17     5     NA   -1.61         NA constant
      298          298 <NA>        17     6     NA   -1.61         NA constant
      299          299 <NA>        17     7     NA   -1.61         NA constant
      300          300 <NA>        17     8     NA   -1.61         NA constant
      301          301 <NA>        17     9     NA   -1.61         NA constant
      302          302 <NA>        17    10     NA   -1.61         NA constant
      303          303 <NA>        17    11     NA   -1.61         NA constant
      304          304 <NA>        17    12     NA   -1.61         NA constant
      305          305 <NA>        18     1     NA   -1.61         NA constant
      306          306 <NA>        18     2     NA   -1.61         NA constant
      307          307 <NA>        18     3     NA   -1.61         NA constant
      308          308 <NA>        18     4     NA   -1.61         NA constant
      309          309 <NA>        18     5     NA   -1.61         NA constant
      310          310 <NA>        18     6     NA   -1.61         NA constant
      311          311 <NA>        18     7     NA   -1.61         NA constant
      312          312 <NA>        18     8     NA   -1.61         NA constant
      313          313 <NA>        18     9     NA   -1.61         NA constant
      314          314 <NA>        18    10     NA   -1.61         NA constant
      315          315 <NA>        18    11     NA   -1.61         NA constant
      316          316 <NA>        18    12     NA   -1.61         NA constant
      317          317 <NA>        19     1     NA   -1.61         NA constant
      318          318 <NA>        19     2     NA   -1.61         NA constant
      319          319 <NA>        19     3     NA   -1.61         NA constant
      320          320 <NA>        19     4     NA   -1.61         NA constant
          distribution input_type  lpdf log_sd
          <chr>        <chr>      <dbl>  <dbl>
        1 <NA>         <NA>       NA    NA
        2 <NA>         <NA>       NA    NA
        3 <NA>         <NA>       NA    NA
        4 <NA>         <NA>       NA    NA
        5 <NA>         <NA>       NA    NA
        6 <NA>         <NA>       NA    NA
        7 <NA>         <NA>       NA    NA
        8 <NA>         <NA>       NA    NA
        9 <NA>         <NA>       NA    NA
       10 <NA>         <NA>       NA    NA
       11 <NA>         <NA>       NA    NA
       12 <NA>         <NA>       NA    NA
       13 <NA>         <NA>       NA    NA
       14 <NA>         <NA>       NA    NA
       15 <NA>         <NA>       NA    NA
       16 <NA>         <NA>       NA    NA
       17 <NA>         <NA>       NA    NA
       18 <NA>         <NA>       NA    NA
       19 <NA>         <NA>       NA    NA
       20 <NA>         <NA>       NA    NA
       21 <NA>         <NA>       NA    NA
       22 <NA>         <NA>       NA    NA
       23 <NA>         <NA>       NA    NA
       24 <NA>         <NA>       NA    NA
       25 <NA>         <NA>       NA    NA
       26 <NA>         <NA>       NA    NA
       27 <NA>         <NA>       NA    NA
       28 <NA>         <NA>       NA    NA
       29 <NA>         <NA>       NA    NA
       30 <NA>         <NA>       NA    NA
       31 <NA>         <NA>       NA    NA
       32 <NA>         <NA>       NA    NA
       33 <NA>         <NA>       NA    NA
       34 <NA>         <NA>       NA    NA
       35 <NA>         <NA>       NA    NA
       36 <NA>         <NA>       NA    NA
       37 <NA>         <NA>       NA    NA
       38 <NA>         <NA>       NA    NA
       39 <NA>         <NA>       NA    NA
       40 <NA>         <NA>       NA    NA
       41 <NA>         <NA>       NA    NA
       42 <NA>         <NA>       NA    NA
       43 <NA>         <NA>       NA    NA
       44 <NA>         <NA>       NA    NA
       45 <NA>         <NA>       NA    NA
       46 <NA>         <NA>       NA    NA
       47 <NA>         <NA>       NA    NA
       48 <NA>         <NA>       NA    NA
       49 <NA>         <NA>       NA    NA
       50 <NA>         <NA>       NA    NA
       51 <NA>         <NA>       NA    NA
       52 <NA>         <NA>       NA    NA
       53 <NA>         <NA>       NA    NA
       54 <NA>         <NA>       NA    NA
       55 <NA>         <NA>       NA    NA
       56 <NA>         <NA>       NA    NA
       57 <NA>         <NA>       NA    NA
       58 <NA>         <NA>       NA    NA
       59 <NA>         <NA>       NA    NA
       60 <NA>         <NA>       NA    NA
       61 <NA>         <NA>       NA    NA
       62 <NA>         <NA>       NA    NA
       63 <NA>         <NA>       NA    NA
       64 <NA>         <NA>       NA    NA
       65 <NA>         <NA>       NA    NA
       66 <NA>         <NA>       NA    NA
       67 <NA>         <NA>       NA    NA
       68 <NA>         <NA>       NA    NA
       69 <NA>         <NA>       -9.91 -0.916
       70 <NA>         <NA>       -9.91 -0.916
       71 <NA>         <NA>       -9.91 -0.916
       72 <NA>         <NA>       -9.91 -0.916
       73 <NA>         <NA>       -9.91 -0.916
       74 <NA>         <NA>       -9.91 -0.916
       75 <NA>         <NA>       -9.91 -0.916
       76 <NA>         <NA>       -9.91 -0.916
       77 <NA>         <NA>       -9.91 -0.916
       78 <NA>         <NA>       -9.91 -0.916
       79 <NA>         <NA>       -9.91 -0.916
       80 <NA>         <NA>       -9.91 -0.916
       81 <NA>         <NA>       -9.91 -0.916
       82 <NA>         <NA>       -9.91 -0.916
       83 <NA>         <NA>       -9.91 -0.916
       84 <NA>         <NA>       -9.91 -0.916
       85 <NA>         <NA>       -9.91 -0.916
       86 <NA>         <NA>       -9.91 -0.916
       87 <NA>         <NA>       -9.91 -0.916
       88 <NA>         <NA>       -9.91 -0.916
       89 <NA>         <NA>       -9.91 -0.916
       90 <NA>         <NA>       -9.91 -0.916
       91 <NA>         <NA>       -9.91 -0.916
       92 <NA>         <NA>       -9.91 -0.916
       93 <NA>         <NA>       -9.91 -0.916
       94 <NA>         <NA>       -9.91 -0.916
       95 <NA>         <NA>       -9.91 -0.916
       96 <NA>         <NA>       -9.91 -0.916
       97 <NA>         <NA>       -9.91 -0.916
       98 normal       <NA>       NA    NA
       99 <NA>         <NA>       NA    NA
      100 <NA>         <NA>       NA    NA
      101 <NA>         <NA>       NA    NA
      102 <NA>         <NA>       NA    NA
      103 <NA>         <NA>       NA    NA
      104 <NA>         <NA>       NA    NA
      105 <NA>         <NA>       NA    NA
      106 <NA>         <NA>       NA    NA
      107 <NA>         <NA>       NA    NA
      108 <NA>         <NA>       NA    NA
      109 <NA>         <NA>       NA    NA
      110 <NA>         <NA>       NA    NA
      111 <NA>         <NA>       NA    NA
      112 <NA>         <NA>       NA    NA
      113 <NA>         <NA>       NA    NA
      114 <NA>         <NA>       NA    NA
      115 <NA>         <NA>       NA    NA
      116 <NA>         <NA>       NA    NA
      117 <NA>         <NA>       NA    NA
      118 <NA>         <NA>       NA    NA
      119 <NA>         <NA>       NA    NA
      120 <NA>         <NA>       NA    NA
      121 <NA>         <NA>       NA    NA
      122 <NA>         <NA>       NA    NA
      123 <NA>         <NA>       NA    NA
      124 <NA>         <NA>       NA    NA
      125 <NA>         <NA>       NA    NA
      126 <NA>         <NA>       NA    NA
      127 <NA>         <NA>       NA    NA
      128 <NA>         <NA>       NA    NA
      129 <NA>         <NA>       NA    NA
      130 <NA>         <NA>       NA    NA
      131 <NA>         <NA>       NA    NA
      132 <NA>         <NA>       NA    NA
      133 <NA>         <NA>       NA    NA
      134 <NA>         <NA>       NA    NA
      135 <NA>         <NA>       NA    NA
      136 <NA>         <NA>       NA    NA
      137 <NA>         <NA>       NA    NA
      138 <NA>         <NA>       NA    NA
      139 <NA>         <NA>       NA    NA
      140 <NA>         <NA>       NA    NA
      141 <NA>         <NA>       NA    NA
      142 <NA>         <NA>       NA    NA
      143 <NA>         <NA>       NA    NA
      144 <NA>         <NA>       NA    NA
      145 <NA>         <NA>       NA    NA
      146 <NA>         <NA>       NA    NA
      147 <NA>         <NA>       NA    NA
      148 <NA>         <NA>       NA    NA
      149 <NA>         <NA>       NA    NA
      150 <NA>         <NA>       NA    NA
      151 <NA>         <NA>       NA    NA
      152 <NA>         <NA>       NA    NA
      153 <NA>         <NA>       NA    NA
      154 <NA>         <NA>       NA    NA
      155 <NA>         <NA>       NA    NA
      156 <NA>         <NA>       NA    NA
      157 <NA>         <NA>       NA    NA
      158 <NA>         <NA>       NA    NA
      159 <NA>         <NA>       NA    NA
      160 <NA>         <NA>       NA    NA
      161 <NA>         <NA>       NA    NA
      162 <NA>         <NA>       NA    NA
      163 <NA>         <NA>       NA    NA
      164 <NA>         <NA>       NA    NA
      165 <NA>         <NA>       NA    NA
      166 <NA>         <NA>       NA    NA
      167 <NA>         <NA>       NA    NA
      168 <NA>         <NA>       NA    NA
      169 <NA>         <NA>       NA    NA
      170 <NA>         <NA>       NA    NA
      171 <NA>         <NA>       NA    NA
      172 <NA>         <NA>       NA    NA
      173 <NA>         <NA>       NA    NA
      174 <NA>         <NA>       NA    NA
      175 <NA>         <NA>       NA    NA
      176 <NA>         <NA>       NA    NA
      177 <NA>         <NA>       NA    NA
      178 <NA>         <NA>       NA    NA
      179 <NA>         <NA>       NA    NA
      180 <NA>         <NA>       NA    NA
      181 <NA>         <NA>       NA    NA
      182 <NA>         <NA>       NA    NA
      183 <NA>         <NA>       NA    NA
      184 <NA>         <NA>       NA    NA
      185 <NA>         <NA>       NA    NA
      186 <NA>         <NA>       NA    NA
      187 <NA>         <NA>       NA    NA
      188 <NA>         <NA>       NA    NA
      189 <NA>         <NA>       NA    NA
      190 <NA>         <NA>       NA    NA
      191 <NA>         <NA>       NA    NA
      192 <NA>         <NA>       NA    NA
      193 <NA>         <NA>       NA    NA
      194 <NA>         <NA>       NA    NA
      195 <NA>         <NA>       NA    NA
      196 <NA>         <NA>       NA    NA
      197 <NA>         <NA>       NA    NA
      198 <NA>         <NA>       NA    NA
      199 <NA>         <NA>       NA    NA
      200 <NA>         <NA>       NA    NA
      201 <NA>         <NA>       NA    NA
      202 <NA>         <NA>       NA    NA
      203 <NA>         <NA>       NA    NA
      204 <NA>         <NA>       NA    NA
      205 <NA>         <NA>       NA    NA
      206 <NA>         <NA>       NA    NA
      207 <NA>         <NA>       NA    NA
      208 <NA>         <NA>       NA    NA
      209 <NA>         <NA>       NA    NA
      210 <NA>         <NA>       NA    NA
      211 <NA>         <NA>       NA    NA
      212 <NA>         <NA>       NA    NA
      213 <NA>         <NA>       NA    NA
      214 <NA>         <NA>       NA    NA
      215 <NA>         <NA>       NA    NA
      216 <NA>         <NA>       NA    NA
      217 <NA>         <NA>       NA    NA
      218 <NA>         <NA>       NA    NA
      219 <NA>         <NA>       NA    NA
      220 <NA>         <NA>       NA    NA
      221 <NA>         <NA>       NA    NA
      222 <NA>         <NA>       NA    NA
      223 <NA>         <NA>       NA    NA
      224 <NA>         <NA>       NA    NA
      225 <NA>         <NA>       NA    NA
      226 <NA>         <NA>       NA    NA
      227 <NA>         <NA>       NA    NA
      228 <NA>         <NA>       NA    NA
      229 <NA>         <NA>       NA    NA
      230 <NA>         <NA>       NA    NA
      231 <NA>         <NA>       NA    NA
      232 <NA>         <NA>       NA    NA
      233 <NA>         <NA>       NA    NA
      234 <NA>         <NA>       NA    NA
      235 <NA>         <NA>       NA    NA
      236 <NA>         <NA>       NA    NA
      237 <NA>         <NA>       NA    NA
      238 <NA>         <NA>       NA    NA
      239 <NA>         <NA>       NA    NA
      240 <NA>         <NA>       NA    NA
      241 <NA>         <NA>       NA    NA
      242 <NA>         <NA>       NA    NA
      243 <NA>         <NA>       NA    NA
      244 <NA>         <NA>       NA    NA
      245 <NA>         <NA>       NA    NA
      246 <NA>         <NA>       NA    NA
      247 <NA>         <NA>       NA    NA
      248 <NA>         <NA>       NA    NA
      249 <NA>         <NA>       NA    NA
      250 <NA>         <NA>       NA    NA
      251 <NA>         <NA>       NA    NA
      252 <NA>         <NA>       NA    NA
      253 <NA>         <NA>       NA    NA
      254 <NA>         <NA>       NA    NA
      255 <NA>         <NA>       NA    NA
      256 <NA>         <NA>       NA    NA
      257 <NA>         <NA>       NA    NA
      258 <NA>         <NA>       NA    NA
      259 <NA>         <NA>       NA    NA
      260 <NA>         <NA>       NA    NA
      261 <NA>         <NA>       NA    NA
      262 <NA>         <NA>       NA    NA
      263 <NA>         <NA>       NA    NA
      264 <NA>         <NA>       NA    NA
      265 <NA>         <NA>       NA    NA
      266 <NA>         <NA>       NA    NA
      267 <NA>         <NA>       NA    NA
      268 <NA>         <NA>       NA    NA
      269 <NA>         <NA>       NA    NA
      270 <NA>         <NA>       NA    NA
      271 <NA>         <NA>       NA    NA
      272 <NA>         <NA>       NA    NA
      273 <NA>         <NA>       NA    NA
      274 <NA>         <NA>       NA    NA
      275 <NA>         <NA>       NA    NA
      276 <NA>         <NA>       NA    NA
      277 <NA>         <NA>       NA    NA
      278 <NA>         <NA>       NA    NA
      279 <NA>         <NA>       NA    NA
      280 <NA>         <NA>       NA    NA
      281 <NA>         <NA>       NA    NA
      282 <NA>         <NA>       NA    NA
      283 <NA>         <NA>       NA    NA
      284 <NA>         <NA>       NA    NA
      285 <NA>         <NA>       NA    NA
      286 <NA>         <NA>       NA    NA
      287 <NA>         <NA>       NA    NA
      288 <NA>         <NA>       NA    NA
      289 <NA>         <NA>       NA    NA
      290 <NA>         <NA>       NA    NA
      291 <NA>         <NA>       NA    NA
      292 <NA>         <NA>       NA    NA
      293 <NA>         <NA>       NA    NA
      294 <NA>         <NA>       NA    NA
      295 <NA>         <NA>       NA    NA
      296 <NA>         <NA>       NA    NA
      297 <NA>         <NA>       NA    NA
      298 <NA>         <NA>       NA    NA
      299 <NA>         <NA>       NA    NA
      300 <NA>         <NA>       NA    NA
      301 <NA>         <NA>       NA    NA
      302 <NA>         <NA>       NA    NA
      303 <NA>         <NA>       NA    NA
      304 <NA>         <NA>       NA    NA
      305 <NA>         <NA>       NA    NA
      306 <NA>         <NA>       NA    NA
      307 <NA>         <NA>       NA    NA
      308 <NA>         <NA>       NA    NA
      309 <NA>         <NA>       NA    NA
      310 <NA>         <NA>       NA    NA
      311 <NA>         <NA>       NA    NA
      312 <NA>         <NA>       NA    NA
      313 <NA>         <NA>       NA    NA
      314 <NA>         <NA>       NA    NA
      315 <NA>         <NA>       NA    NA
      316 <NA>         <NA>       NA    NA
      317 <NA>         <NA>       NA    NA
      318 <NA>         <NA>       NA    NA
      319 <NA>         <NA>       NA    NA
      320 <NA>         <NA>       NA    NA
      # i 13,647 more rows

# `get_estimates()` works with estimation run

    Code
      estimates_snapshot <- dplyr::select(estimates_big_optimized, -estimated,
        -expected, -uncertainty, -gradient, -likelihood, -log_like_cv)
      estimates_snapshot_lines <- capture.output(print(estimates_snapshot, n = 320,
        width = Inf))
      cat(trimws(estimates_snapshot_lines, which = "right"), sep = "\n")
    Output
      # A tibble: 13,967 x 18
          module_name module_id module_type  label            type   type_id
          <chr>           <int> <chr>        <chr>            <chr>    <int>
        1 Selectivity         1 Logistic     inflection_point vector      NA
        2 Selectivity         1 Logistic     slope            vector      NA
        3 Selectivity         2 Logistic     inflection_point vector      NA
        4 Selectivity         2 Logistic     slope            vector      NA
        5 Fleet               1 <NA>         log_q            vector      NA
        6 Fleet               1 <NA>         log_Fmort        vector      NA
        7 Fleet               1 <NA>         log_Fmort        vector      NA
        8 Fleet               1 <NA>         log_Fmort        vector      NA
        9 Fleet               1 <NA>         log_Fmort        vector      NA
       10 Fleet               1 <NA>         log_Fmort        vector      NA
       11 Fleet               1 <NA>         log_Fmort        vector      NA
       12 Fleet               1 <NA>         log_Fmort        vector      NA
       13 Fleet               1 <NA>         log_Fmort        vector      NA
       14 Fleet               1 <NA>         log_Fmort        vector      NA
       15 Fleet               1 <NA>         log_Fmort        vector      NA
       16 Fleet               1 <NA>         log_Fmort        vector      NA
       17 Fleet               1 <NA>         log_Fmort        vector      NA
       18 Fleet               1 <NA>         log_Fmort        vector      NA
       19 Fleet               1 <NA>         log_Fmort        vector      NA
       20 Fleet               1 <NA>         log_Fmort        vector      NA
       21 Fleet               1 <NA>         log_Fmort        vector      NA
       22 Fleet               1 <NA>         log_Fmort        vector      NA
       23 Fleet               1 <NA>         log_Fmort        vector      NA
       24 Fleet               1 <NA>         log_Fmort        vector      NA
       25 Fleet               1 <NA>         log_Fmort        vector      NA
       26 Fleet               1 <NA>         log_Fmort        vector      NA
       27 Fleet               1 <NA>         log_Fmort        vector      NA
       28 Fleet               1 <NA>         log_Fmort        vector      NA
       29 Fleet               1 <NA>         log_Fmort        vector      NA
       30 Fleet               1 <NA>         log_Fmort        vector      NA
       31 Fleet               1 <NA>         log_Fmort        vector      NA
       32 Fleet               1 <NA>         log_Fmort        vector      NA
       33 Fleet               1 <NA>         log_Fmort        vector      NA
       34 Fleet               1 <NA>         log_Fmort        vector      NA
       35 Fleet               1 <NA>         log_Fmort        vector      NA
       36 Fleet               2 <NA>         log_q            vector      NA
       37 Fleet               2 <NA>         log_Fmort        vector      NA
       38 Fleet               2 <NA>         log_Fmort        vector      NA
       39 Fleet               2 <NA>         log_Fmort        vector      NA
       40 Fleet               2 <NA>         log_Fmort        vector      NA
       41 Fleet               2 <NA>         log_Fmort        vector      NA
       42 Fleet               2 <NA>         log_Fmort        vector      NA
       43 Fleet               2 <NA>         log_Fmort        vector      NA
       44 Fleet               2 <NA>         log_Fmort        vector      NA
       45 Fleet               2 <NA>         log_Fmort        vector      NA
       46 Fleet               2 <NA>         log_Fmort        vector      NA
       47 Fleet               2 <NA>         log_Fmort        vector      NA
       48 Fleet               2 <NA>         log_Fmort        vector      NA
       49 Fleet               2 <NA>         log_Fmort        vector      NA
       50 Fleet               2 <NA>         log_Fmort        vector      NA
       51 Fleet               2 <NA>         log_Fmort        vector      NA
       52 Fleet               2 <NA>         log_Fmort        vector      NA
       53 Fleet               2 <NA>         log_Fmort        vector      NA
       54 Fleet               2 <NA>         log_Fmort        vector      NA
       55 Fleet               2 <NA>         log_Fmort        vector      NA
       56 Fleet               2 <NA>         log_Fmort        vector      NA
       57 Fleet               2 <NA>         log_Fmort        vector      NA
       58 Fleet               2 <NA>         log_Fmort        vector      NA
       59 Fleet               2 <NA>         log_Fmort        vector      NA
       60 Fleet               2 <NA>         log_Fmort        vector      NA
       61 Fleet               2 <NA>         log_Fmort        vector      NA
       62 Fleet               2 <NA>         log_Fmort        vector      NA
       63 Fleet               2 <NA>         log_Fmort        vector      NA
       64 Fleet               2 <NA>         log_Fmort        vector      NA
       65 Fleet               2 <NA>         log_Fmort        vector      NA
       66 Fleet               2 <NA>         log_Fmort        vector      NA
       67 Recruitment         1 BevertonHolt log_rzero        vector      NA
       68 Recruitment         1 BevertonHolt logit_steep      vector      NA
       69 Recruitment         1 BevertonHolt log_devs         vector      NA
       70 Recruitment         1 BevertonHolt log_devs         vector      NA
       71 Recruitment         1 BevertonHolt log_devs         vector      NA
       72 Recruitment         1 BevertonHolt log_devs         vector      NA
       73 Recruitment         1 BevertonHolt log_devs         vector      NA
       74 Recruitment         1 BevertonHolt log_devs         vector      NA
       75 Recruitment         1 BevertonHolt log_devs         vector      NA
       76 Recruitment         1 BevertonHolt log_devs         vector      NA
       77 Recruitment         1 BevertonHolt log_devs         vector      NA
       78 Recruitment         1 BevertonHolt log_devs         vector      NA
       79 Recruitment         1 BevertonHolt log_devs         vector      NA
       80 Recruitment         1 BevertonHolt log_devs         vector      NA
       81 Recruitment         1 BevertonHolt log_devs         vector      NA
       82 Recruitment         1 BevertonHolt log_devs         vector      NA
       83 Recruitment         1 BevertonHolt log_devs         vector      NA
       84 Recruitment         1 BevertonHolt log_devs         vector      NA
       85 Recruitment         1 BevertonHolt log_devs         vector      NA
       86 Recruitment         1 BevertonHolt log_devs         vector      NA
       87 Recruitment         1 BevertonHolt log_devs         vector      NA
       88 Recruitment         1 BevertonHolt log_devs         vector      NA
       89 Recruitment         1 BevertonHolt log_devs         vector      NA
       90 Recruitment         1 BevertonHolt log_devs         vector      NA
       91 Recruitment         1 BevertonHolt log_devs         vector      NA
       92 Recruitment         1 BevertonHolt log_devs         vector      NA
       93 Recruitment         1 BevertonHolt log_devs         vector      NA
       94 Recruitment         1 BevertonHolt log_devs         vector      NA
       95 Recruitment         1 BevertonHolt log_devs         vector      NA
       96 Recruitment         1 BevertonHolt log_devs         vector      NA
       97 Recruitment         1 BevertonHolt log_devs         vector      NA
       98 Recruitment         1 BevertonHolt log_sd           vector      NA
       99 Maturity            1 Logistic     inflection_point vector      NA
      100 Maturity            1 Logistic     slope            vector      NA
      101 Population         NA <NA>         log_M            vector      NA
      102 Population         NA <NA>         log_M            vector      NA
      103 Population         NA <NA>         log_M            vector      NA
      104 Population         NA <NA>         log_M            vector      NA
      105 Population         NA <NA>         log_M            vector      NA
      106 Population         NA <NA>         log_M            vector      NA
      107 Population         NA <NA>         log_M            vector      NA
      108 Population         NA <NA>         log_M            vector      NA
      109 Population         NA <NA>         log_M            vector      NA
      110 Population         NA <NA>         log_M            vector      NA
      111 Population         NA <NA>         log_M            vector      NA
      112 Population         NA <NA>         log_M            vector      NA
      113 Population         NA <NA>         log_M            vector      NA
      114 Population         NA <NA>         log_M            vector      NA
      115 Population         NA <NA>         log_M            vector      NA
      116 Population         NA <NA>         log_M            vector      NA
      117 Population         NA <NA>         log_M            vector      NA
      118 Population         NA <NA>         log_M            vector      NA
      119 Population         NA <NA>         log_M            vector      NA
      120 Population         NA <NA>         log_M            vector      NA
      121 Population         NA <NA>         log_M            vector      NA
      122 Population         NA <NA>         log_M            vector      NA
      123 Population         NA <NA>         log_M            vector      NA
      124 Population         NA <NA>         log_M            vector      NA
      125 Population         NA <NA>         log_M            vector      NA
      126 Population         NA <NA>         log_M            vector      NA
      127 Population         NA <NA>         log_M            vector      NA
      128 Population         NA <NA>         log_M            vector      NA
      129 Population         NA <NA>         log_M            vector      NA
      130 Population         NA <NA>         log_M            vector      NA
      131 Population         NA <NA>         log_M            vector      NA
      132 Population         NA <NA>         log_M            vector      NA
      133 Population         NA <NA>         log_M            vector      NA
      134 Population         NA <NA>         log_M            vector      NA
      135 Population         NA <NA>         log_M            vector      NA
      136 Population         NA <NA>         log_M            vector      NA
      137 Population         NA <NA>         log_M            vector      NA
      138 Population         NA <NA>         log_M            vector      NA
      139 Population         NA <NA>         log_M            vector      NA
      140 Population         NA <NA>         log_M            vector      NA
      141 Population         NA <NA>         log_M            vector      NA
      142 Population         NA <NA>         log_M            vector      NA
      143 Population         NA <NA>         log_M            vector      NA
      144 Population         NA <NA>         log_M            vector      NA
      145 Population         NA <NA>         log_M            vector      NA
      146 Population         NA <NA>         log_M            vector      NA
      147 Population         NA <NA>         log_M            vector      NA
      148 Population         NA <NA>         log_M            vector      NA
      149 Population         NA <NA>         log_M            vector      NA
      150 Population         NA <NA>         log_M            vector      NA
      151 Population         NA <NA>         log_M            vector      NA
      152 Population         NA <NA>         log_M            vector      NA
      153 Population         NA <NA>         log_M            vector      NA
      154 Population         NA <NA>         log_M            vector      NA
      155 Population         NA <NA>         log_M            vector      NA
      156 Population         NA <NA>         log_M            vector      NA
      157 Population         NA <NA>         log_M            vector      NA
      158 Population         NA <NA>         log_M            vector      NA
      159 Population         NA <NA>         log_M            vector      NA
      160 Population         NA <NA>         log_M            vector      NA
      161 Population         NA <NA>         log_M            vector      NA
      162 Population         NA <NA>         log_M            vector      NA
      163 Population         NA <NA>         log_M            vector      NA
      164 Population         NA <NA>         log_M            vector      NA
      165 Population         NA <NA>         log_M            vector      NA
      166 Population         NA <NA>         log_M            vector      NA
      167 Population         NA <NA>         log_M            vector      NA
      168 Population         NA <NA>         log_M            vector      NA
      169 Population         NA <NA>         log_M            vector      NA
      170 Population         NA <NA>         log_M            vector      NA
      171 Population         NA <NA>         log_M            vector      NA
      172 Population         NA <NA>         log_M            vector      NA
      173 Population         NA <NA>         log_M            vector      NA
      174 Population         NA <NA>         log_M            vector      NA
      175 Population         NA <NA>         log_M            vector      NA
      176 Population         NA <NA>         log_M            vector      NA
      177 Population         NA <NA>         log_M            vector      NA
      178 Population         NA <NA>         log_M            vector      NA
      179 Population         NA <NA>         log_M            vector      NA
      180 Population         NA <NA>         log_M            vector      NA
      181 Population         NA <NA>         log_M            vector      NA
      182 Population         NA <NA>         log_M            vector      NA
      183 Population         NA <NA>         log_M            vector      NA
      184 Population         NA <NA>         log_M            vector      NA
      185 Population         NA <NA>         log_M            vector      NA
      186 Population         NA <NA>         log_M            vector      NA
      187 Population         NA <NA>         log_M            vector      NA
      188 Population         NA <NA>         log_M            vector      NA
      189 Population         NA <NA>         log_M            vector      NA
      190 Population         NA <NA>         log_M            vector      NA
      191 Population         NA <NA>         log_M            vector      NA
      192 Population         NA <NA>         log_M            vector      NA
      193 Population         NA <NA>         log_M            vector      NA
      194 Population         NA <NA>         log_M            vector      NA
      195 Population         NA <NA>         log_M            vector      NA
      196 Population         NA <NA>         log_M            vector      NA
      197 Population         NA <NA>         log_M            vector      NA
      198 Population         NA <NA>         log_M            vector      NA
      199 Population         NA <NA>         log_M            vector      NA
      200 Population         NA <NA>         log_M            vector      NA
      201 Population         NA <NA>         log_M            vector      NA
      202 Population         NA <NA>         log_M            vector      NA
      203 Population         NA <NA>         log_M            vector      NA
      204 Population         NA <NA>         log_M            vector      NA
      205 Population         NA <NA>         log_M            vector      NA
      206 Population         NA <NA>         log_M            vector      NA
      207 Population         NA <NA>         log_M            vector      NA
      208 Population         NA <NA>         log_M            vector      NA
      209 Population         NA <NA>         log_M            vector      NA
      210 Population         NA <NA>         log_M            vector      NA
      211 Population         NA <NA>         log_M            vector      NA
      212 Population         NA <NA>         log_M            vector      NA
      213 Population         NA <NA>         log_M            vector      NA
      214 Population         NA <NA>         log_M            vector      NA
      215 Population         NA <NA>         log_M            vector      NA
      216 Population         NA <NA>         log_M            vector      NA
      217 Population         NA <NA>         log_M            vector      NA
      218 Population         NA <NA>         log_M            vector      NA
      219 Population         NA <NA>         log_M            vector      NA
      220 Population         NA <NA>         log_M            vector      NA
      221 Population         NA <NA>         log_M            vector      NA
      222 Population         NA <NA>         log_M            vector      NA
      223 Population         NA <NA>         log_M            vector      NA
      224 Population         NA <NA>         log_M            vector      NA
      225 Population         NA <NA>         log_M            vector      NA
      226 Population         NA <NA>         log_M            vector      NA
      227 Population         NA <NA>         log_M            vector      NA
      228 Population         NA <NA>         log_M            vector      NA
      229 Population         NA <NA>         log_M            vector      NA
      230 Population         NA <NA>         log_M            vector      NA
      231 Population         NA <NA>         log_M            vector      NA
      232 Population         NA <NA>         log_M            vector      NA
      233 Population         NA <NA>         log_M            vector      NA
      234 Population         NA <NA>         log_M            vector      NA
      235 Population         NA <NA>         log_M            vector      NA
      236 Population         NA <NA>         log_M            vector      NA
      237 Population         NA <NA>         log_M            vector      NA
      238 Population         NA <NA>         log_M            vector      NA
      239 Population         NA <NA>         log_M            vector      NA
      240 Population         NA <NA>         log_M            vector      NA
      241 Population         NA <NA>         log_M            vector      NA
      242 Population         NA <NA>         log_M            vector      NA
      243 Population         NA <NA>         log_M            vector      NA
      244 Population         NA <NA>         log_M            vector      NA
      245 Population         NA <NA>         log_M            vector      NA
      246 Population         NA <NA>         log_M            vector      NA
      247 Population         NA <NA>         log_M            vector      NA
      248 Population         NA <NA>         log_M            vector      NA
      249 Population         NA <NA>         log_M            vector      NA
      250 Population         NA <NA>         log_M            vector      NA
      251 Population         NA <NA>         log_M            vector      NA
      252 Population         NA <NA>         log_M            vector      NA
      253 Population         NA <NA>         log_M            vector      NA
      254 Population         NA <NA>         log_M            vector      NA
      255 Population         NA <NA>         log_M            vector      NA
      256 Population         NA <NA>         log_M            vector      NA
      257 Population         NA <NA>         log_M            vector      NA
      258 Population         NA <NA>         log_M            vector      NA
      259 Population         NA <NA>         log_M            vector      NA
      260 Population         NA <NA>         log_M            vector      NA
      261 Population         NA <NA>         log_M            vector      NA
      262 Population         NA <NA>         log_M            vector      NA
      263 Population         NA <NA>         log_M            vector      NA
      264 Population         NA <NA>         log_M            vector      NA
      265 Population         NA <NA>         log_M            vector      NA
      266 Population         NA <NA>         log_M            vector      NA
      267 Population         NA <NA>         log_M            vector      NA
      268 Population         NA <NA>         log_M            vector      NA
      269 Population         NA <NA>         log_M            vector      NA
      270 Population         NA <NA>         log_M            vector      NA
      271 Population         NA <NA>         log_M            vector      NA
      272 Population         NA <NA>         log_M            vector      NA
      273 Population         NA <NA>         log_M            vector      NA
      274 Population         NA <NA>         log_M            vector      NA
      275 Population         NA <NA>         log_M            vector      NA
      276 Population         NA <NA>         log_M            vector      NA
      277 Population         NA <NA>         log_M            vector      NA
      278 Population         NA <NA>         log_M            vector      NA
      279 Population         NA <NA>         log_M            vector      NA
      280 Population         NA <NA>         log_M            vector      NA
      281 Population         NA <NA>         log_M            vector      NA
      282 Population         NA <NA>         log_M            vector      NA
      283 Population         NA <NA>         log_M            vector      NA
      284 Population         NA <NA>         log_M            vector      NA
      285 Population         NA <NA>         log_M            vector      NA
      286 Population         NA <NA>         log_M            vector      NA
      287 Population         NA <NA>         log_M            vector      NA
      288 Population         NA <NA>         log_M            vector      NA
      289 Population         NA <NA>         log_M            vector      NA
      290 Population         NA <NA>         log_M            vector      NA
      291 Population         NA <NA>         log_M            vector      NA
      292 Population         NA <NA>         log_M            vector      NA
      293 Population         NA <NA>         log_M            vector      NA
      294 Population         NA <NA>         log_M            vector      NA
      295 Population         NA <NA>         log_M            vector      NA
      296 Population         NA <NA>         log_M            vector      NA
      297 Population         NA <NA>         log_M            vector      NA
      298 Population         NA <NA>         log_M            vector      NA
      299 Population         NA <NA>         log_M            vector      NA
      300 Population         NA <NA>         log_M            vector      NA
      301 Population         NA <NA>         log_M            vector      NA
      302 Population         NA <NA>         log_M            vector      NA
      303 Population         NA <NA>         log_M            vector      NA
      304 Population         NA <NA>         log_M            vector      NA
      305 Population         NA <NA>         log_M            vector      NA
      306 Population         NA <NA>         log_M            vector      NA
      307 Population         NA <NA>         log_M            vector      NA
      308 Population         NA <NA>         log_M            vector      NA
      309 Population         NA <NA>         log_M            vector      NA
      310 Population         NA <NA>         log_M            vector      NA
      311 Population         NA <NA>         log_M            vector      NA
      312 Population         NA <NA>         log_M            vector      NA
      313 Population         NA <NA>         log_M            vector      NA
      314 Population         NA <NA>         log_M            vector      NA
      315 Population         NA <NA>         log_M            vector      NA
      316 Population         NA <NA>         log_M            vector      NA
      317 Population         NA <NA>         log_M            vector      NA
      318 Population         NA <NA>         log_M            vector      NA
      319 Population         NA <NA>         log_M            vector      NA
      320 Population         NA <NA>         log_M            vector      NA
          parameter_id fleet   year_i   age length     input observed estimation_type
                 <int> <chr>    <dbl> <dbl>  <dbl>     <dbl>    <dbl> <chr>
        1            1 fleet1      NA    NA     NA    2            NA fixed_effects
        2            2 fleet1      NA    NA     NA    1            NA fixed_effects
        3            3 survey1     NA    NA     NA    1.5          NA fixed_effects
        4            4 survey1     NA    NA     NA    2            NA fixed_effects
        5            5 fleet1      NA    NA     NA    0            NA constant
        6            6 fleet1       1    NA     NA   -4.66         NA fixed_effects
        7            7 fleet1       2    NA     NA   -3.60         NA fixed_effects
        8            8 fleet1       3    NA     NA   -3.10         NA fixed_effects
        9            9 fleet1       4    NA     NA   -2.80         NA fixed_effects
       10           10 fleet1       5    NA     NA   -3.02         NA fixed_effects
       11           11 fleet1       6    NA     NA   -2.44         NA fixed_effects
       12           12 fleet1       7    NA     NA   -2.43         NA fixed_effects
       13           13 fleet1       8    NA     NA   -1.68         NA fixed_effects
       14           14 fleet1       9    NA     NA   -2.22         NA fixed_effects
       15           15 fleet1      10    NA     NA   -2.02         NA fixed_effects
       16           16 fleet1      11    NA     NA   -1.89         NA fixed_effects
       17           17 fleet1      12    NA     NA   -1.82         NA fixed_effects
       18           18 fleet1      13    NA     NA   -2.15         NA fixed_effects
       19           19 fleet1      14    NA     NA   -1.78         NA fixed_effects
       20           20 fleet1      15    NA     NA   -1.71         NA fixed_effects
       21           21 fleet1      16    NA     NA   -1.82         NA fixed_effects
       22           22 fleet1      17    NA     NA   -1.16         NA fixed_effects
       23           23 fleet1      18    NA     NA   -1.36         NA fixed_effects
       24           24 fleet1      19    NA     NA   -1.37         NA fixed_effects
       25           25 fleet1      20    NA     NA   -1.38         NA fixed_effects
       26           26 fleet1      21    NA     NA   -1.05         NA fixed_effects
       27           27 fleet1      22    NA     NA   -1.37         NA fixed_effects
       28           28 fleet1      23    NA     NA   -0.871        NA fixed_effects
       29           29 fleet1      24    NA     NA   -1.06         NA fixed_effects
       30           30 fleet1      25    NA     NA   -1.07         NA fixed_effects
       31           31 fleet1      26    NA     NA   -1.16         NA fixed_effects
       32           32 fleet1      27    NA     NA   -1.18         NA fixed_effects
       33           33 fleet1      28    NA     NA   -0.840        NA fixed_effects
       34           34 fleet1      29    NA     NA   -1.11         NA fixed_effects
       35           35 fleet1      30    NA     NA   -0.694        NA fixed_effects
       36           36 survey1     NA    NA     NA  -14.9          NA fixed_effects
       37           37 survey1      1    NA     NA -200            NA constant
       38           38 survey1      2    NA     NA -200            NA constant
       39           39 survey1      3    NA     NA -200            NA constant
       40           40 survey1      4    NA     NA -200            NA constant
       41           41 survey1      5    NA     NA -200            NA constant
       42           42 survey1      6    NA     NA -200            NA constant
       43           43 survey1      7    NA     NA -200            NA constant
       44           44 survey1      8    NA     NA -200            NA constant
       45           45 survey1      9    NA     NA -200            NA constant
       46           46 survey1     10    NA     NA -200            NA constant
       47           47 survey1     11    NA     NA -200            NA constant
       48           48 survey1     12    NA     NA -200            NA constant
       49           49 survey1     13    NA     NA -200            NA constant
       50           50 survey1     14    NA     NA -200            NA constant
       51           51 survey1     15    NA     NA -200            NA constant
       52           52 survey1     16    NA     NA -200            NA constant
       53           53 survey1     17    NA     NA -200            NA constant
       54           54 survey1     18    NA     NA -200            NA constant
       55           55 survey1     19    NA     NA -200            NA constant
       56           56 survey1     20    NA     NA -200            NA constant
       57           57 survey1     21    NA     NA -200            NA constant
       58           58 survey1     22    NA     NA -200            NA constant
       59           59 survey1     23    NA     NA -200            NA constant
       60           60 survey1     24    NA     NA -200            NA constant
       61           61 survey1     25    NA     NA -200            NA constant
       62           62 survey1     26    NA     NA -200            NA constant
       63           63 survey1     27    NA     NA -200            NA constant
       64           64 survey1     28    NA     NA -200            NA constant
       65           65 survey1     29    NA     NA -200            NA constant
       66           66 survey1     30    NA     NA -200            NA constant
       67           67 <NA>        NA    NA     NA   13.8          NA fixed_effects
       68           68 <NA>        NA    NA     NA    0.788        NA constant
       69           69 <NA>         2    NA     NA    0.438        NA random_effects
       70           70 <NA>         3    NA     NA   -0.133        NA random_effects
       71           71 <NA>         4    NA     NA   -0.433        NA random_effects
       72           72 <NA>         5    NA     NA    0.649        NA random_effects
       73           73 <NA>         6    NA     NA    0.506        NA random_effects
       74           74 <NA>         7    NA     NA   -0.0696       NA random_effects
       75           75 <NA>         8    NA     NA    0.302        NA random_effects
       76           76 <NA>         9    NA     NA   -0.0826       NA random_effects
       77           77 <NA>        10    NA     NA    0.207        NA random_effects
       78           78 <NA>        11    NA     NA    0.153        NA random_effects
       79           79 <NA>        12    NA     NA   -0.217        NA random_effects
       80           80 <NA>        13    NA     NA   -0.133        NA random_effects
       81           81 <NA>        14    NA     NA    0.112        NA random_effects
       82           82 <NA>        15    NA     NA   -0.107        NA random_effects
       83           83 <NA>        16    NA     NA    0.269        NA random_effects
       84           84 <NA>        17    NA     NA    0.241        NA random_effects
       85           85 <NA>        18    NA     NA   -0.545        NA random_effects
       86           86 <NA>        19    NA     NA   -0.237        NA random_effects
       87           87 <NA>        20    NA     NA   -0.585        NA random_effects
       88           88 <NA>        21    NA     NA    0.301        NA random_effects
       89           89 <NA>        22    NA     NA    0.219        NA random_effects
       90           90 <NA>        23    NA     NA   -0.223        NA random_effects
       91           91 <NA>        24    NA     NA   -0.514        NA random_effects
       92           92 <NA>        25    NA     NA    0.157        NA random_effects
       93           93 <NA>        26    NA     NA   -0.540        NA random_effects
       94           94 <NA>        27    NA     NA   -0.196        NA random_effects
       95           95 <NA>        28    NA     NA    0.201        NA random_effects
       96           96 <NA>        29    NA     NA    0.372        NA random_effects
       97           97 <NA>        30    NA     NA   -0.0716       NA random_effects
       98           98 <NA>        NA    NA     NA   -0.916        NA fixed_effects
       99           99 <NA>        NA    NA     NA    2.25         NA constant
      100          100 <NA>        NA    NA     NA    3            NA constant
      101          101 <NA>         1     1     NA   -1.61         NA constant
      102          102 <NA>         1     2     NA   -1.61         NA constant
      103          103 <NA>         1     3     NA   -1.61         NA constant
      104          104 <NA>         1     4     NA   -1.61         NA constant
      105          105 <NA>         1     5     NA   -1.61         NA constant
      106          106 <NA>         1     6     NA   -1.61         NA constant
      107          107 <NA>         1     7     NA   -1.61         NA constant
      108          108 <NA>         1     8     NA   -1.61         NA constant
      109          109 <NA>         1     9     NA   -1.61         NA constant
      110          110 <NA>         1    10     NA   -1.61         NA constant
      111          111 <NA>         1    11     NA   -1.61         NA constant
      112          112 <NA>         1    12     NA   -1.61         NA constant
      113          113 <NA>         2     1     NA   -1.61         NA constant
      114          114 <NA>         2     2     NA   -1.61         NA constant
      115          115 <NA>         2     3     NA   -1.61         NA constant
      116          116 <NA>         2     4     NA   -1.61         NA constant
      117          117 <NA>         2     5     NA   -1.61         NA constant
      118          118 <NA>         2     6     NA   -1.61         NA constant
      119          119 <NA>         2     7     NA   -1.61         NA constant
      120          120 <NA>         2     8     NA   -1.61         NA constant
      121          121 <NA>         2     9     NA   -1.61         NA constant
      122          122 <NA>         2    10     NA   -1.61         NA constant
      123          123 <NA>         2    11     NA   -1.61         NA constant
      124          124 <NA>         2    12     NA   -1.61         NA constant
      125          125 <NA>         3     1     NA   -1.61         NA constant
      126          126 <NA>         3     2     NA   -1.61         NA constant
      127          127 <NA>         3     3     NA   -1.61         NA constant
      128          128 <NA>         3     4     NA   -1.61         NA constant
      129          129 <NA>         3     5     NA   -1.61         NA constant
      130          130 <NA>         3     6     NA   -1.61         NA constant
      131          131 <NA>         3     7     NA   -1.61         NA constant
      132          132 <NA>         3     8     NA   -1.61         NA constant
      133          133 <NA>         3     9     NA   -1.61         NA constant
      134          134 <NA>         3    10     NA   -1.61         NA constant
      135          135 <NA>         3    11     NA   -1.61         NA constant
      136          136 <NA>         3    12     NA   -1.61         NA constant
      137          137 <NA>         4     1     NA   -1.61         NA constant
      138          138 <NA>         4     2     NA   -1.61         NA constant
      139          139 <NA>         4     3     NA   -1.61         NA constant
      140          140 <NA>         4     4     NA   -1.61         NA constant
      141          141 <NA>         4     5     NA   -1.61         NA constant
      142          142 <NA>         4     6     NA   -1.61         NA constant
      143          143 <NA>         4     7     NA   -1.61         NA constant
      144          144 <NA>         4     8     NA   -1.61         NA constant
      145          145 <NA>         4     9     NA   -1.61         NA constant
      146          146 <NA>         4    10     NA   -1.61         NA constant
      147          147 <NA>         4    11     NA   -1.61         NA constant
      148          148 <NA>         4    12     NA   -1.61         NA constant
      149          149 <NA>         5     1     NA   -1.61         NA constant
      150          150 <NA>         5     2     NA   -1.61         NA constant
      151          151 <NA>         5     3     NA   -1.61         NA constant
      152          152 <NA>         5     4     NA   -1.61         NA constant
      153          153 <NA>         5     5     NA   -1.61         NA constant
      154          154 <NA>         5     6     NA   -1.61         NA constant
      155          155 <NA>         5     7     NA   -1.61         NA constant
      156          156 <NA>         5     8     NA   -1.61         NA constant
      157          157 <NA>         5     9     NA   -1.61         NA constant
      158          158 <NA>         5    10     NA   -1.61         NA constant
      159          159 <NA>         5    11     NA   -1.61         NA constant
      160          160 <NA>         5    12     NA   -1.61         NA constant
      161          161 <NA>         6     1     NA   -1.61         NA constant
      162          162 <NA>         6     2     NA   -1.61         NA constant
      163          163 <NA>         6     3     NA   -1.61         NA constant
      164          164 <NA>         6     4     NA   -1.61         NA constant
      165          165 <NA>         6     5     NA   -1.61         NA constant
      166          166 <NA>         6     6     NA   -1.61         NA constant
      167          167 <NA>         6     7     NA   -1.61         NA constant
      168          168 <NA>         6     8     NA   -1.61         NA constant
      169          169 <NA>         6     9     NA   -1.61         NA constant
      170          170 <NA>         6    10     NA   -1.61         NA constant
      171          171 <NA>         6    11     NA   -1.61         NA constant
      172          172 <NA>         6    12     NA   -1.61         NA constant
      173          173 <NA>         7     1     NA   -1.61         NA constant
      174          174 <NA>         7     2     NA   -1.61         NA constant
      175          175 <NA>         7     3     NA   -1.61         NA constant
      176          176 <NA>         7     4     NA   -1.61         NA constant
      177          177 <NA>         7     5     NA   -1.61         NA constant
      178          178 <NA>         7     6     NA   -1.61         NA constant
      179          179 <NA>         7     7     NA   -1.61         NA constant
      180          180 <NA>         7     8     NA   -1.61         NA constant
      181          181 <NA>         7     9     NA   -1.61         NA constant
      182          182 <NA>         7    10     NA   -1.61         NA constant
      183          183 <NA>         7    11     NA   -1.61         NA constant
      184          184 <NA>         7    12     NA   -1.61         NA constant
      185          185 <NA>         8     1     NA   -1.61         NA constant
      186          186 <NA>         8     2     NA   -1.61         NA constant
      187          187 <NA>         8     3     NA   -1.61         NA constant
      188          188 <NA>         8     4     NA   -1.61         NA constant
      189          189 <NA>         8     5     NA   -1.61         NA constant
      190          190 <NA>         8     6     NA   -1.61         NA constant
      191          191 <NA>         8     7     NA   -1.61         NA constant
      192          192 <NA>         8     8     NA   -1.61         NA constant
      193          193 <NA>         8     9     NA   -1.61         NA constant
      194          194 <NA>         8    10     NA   -1.61         NA constant
      195          195 <NA>         8    11     NA   -1.61         NA constant
      196          196 <NA>         8    12     NA   -1.61         NA constant
      197          197 <NA>         9     1     NA   -1.61         NA constant
      198          198 <NA>         9     2     NA   -1.61         NA constant
      199          199 <NA>         9     3     NA   -1.61         NA constant
      200          200 <NA>         9     4     NA   -1.61         NA constant
      201          201 <NA>         9     5     NA   -1.61         NA constant
      202          202 <NA>         9     6     NA   -1.61         NA constant
      203          203 <NA>         9     7     NA   -1.61         NA constant
      204          204 <NA>         9     8     NA   -1.61         NA constant
      205          205 <NA>         9     9     NA   -1.61         NA constant
      206          206 <NA>         9    10     NA   -1.61         NA constant
      207          207 <NA>         9    11     NA   -1.61         NA constant
      208          208 <NA>         9    12     NA   -1.61         NA constant
      209          209 <NA>        10     1     NA   -1.61         NA constant
      210          210 <NA>        10     2     NA   -1.61         NA constant
      211          211 <NA>        10     3     NA   -1.61         NA constant
      212          212 <NA>        10     4     NA   -1.61         NA constant
      213          213 <NA>        10     5     NA   -1.61         NA constant
      214          214 <NA>        10     6     NA   -1.61         NA constant
      215          215 <NA>        10     7     NA   -1.61         NA constant
      216          216 <NA>        10     8     NA   -1.61         NA constant
      217          217 <NA>        10     9     NA   -1.61         NA constant
      218          218 <NA>        10    10     NA   -1.61         NA constant
      219          219 <NA>        10    11     NA   -1.61         NA constant
      220          220 <NA>        10    12     NA   -1.61         NA constant
      221          221 <NA>        11     1     NA   -1.61         NA constant
      222          222 <NA>        11     2     NA   -1.61         NA constant
      223          223 <NA>        11     3     NA   -1.61         NA constant
      224          224 <NA>        11     4     NA   -1.61         NA constant
      225          225 <NA>        11     5     NA   -1.61         NA constant
      226          226 <NA>        11     6     NA   -1.61         NA constant
      227          227 <NA>        11     7     NA   -1.61         NA constant
      228          228 <NA>        11     8     NA   -1.61         NA constant
      229          229 <NA>        11     9     NA   -1.61         NA constant
      230          230 <NA>        11    10     NA   -1.61         NA constant
      231          231 <NA>        11    11     NA   -1.61         NA constant
      232          232 <NA>        11    12     NA   -1.61         NA constant
      233          233 <NA>        12     1     NA   -1.61         NA constant
      234          234 <NA>        12     2     NA   -1.61         NA constant
      235          235 <NA>        12     3     NA   -1.61         NA constant
      236          236 <NA>        12     4     NA   -1.61         NA constant
      237          237 <NA>        12     5     NA   -1.61         NA constant
      238          238 <NA>        12     6     NA   -1.61         NA constant
      239          239 <NA>        12     7     NA   -1.61         NA constant
      240          240 <NA>        12     8     NA   -1.61         NA constant
      241          241 <NA>        12     9     NA   -1.61         NA constant
      242          242 <NA>        12    10     NA   -1.61         NA constant
      243          243 <NA>        12    11     NA   -1.61         NA constant
      244          244 <NA>        12    12     NA   -1.61         NA constant
      245          245 <NA>        13     1     NA   -1.61         NA constant
      246          246 <NA>        13     2     NA   -1.61         NA constant
      247          247 <NA>        13     3     NA   -1.61         NA constant
      248          248 <NA>        13     4     NA   -1.61         NA constant
      249          249 <NA>        13     5     NA   -1.61         NA constant
      250          250 <NA>        13     6     NA   -1.61         NA constant
      251          251 <NA>        13     7     NA   -1.61         NA constant
      252          252 <NA>        13     8     NA   -1.61         NA constant
      253          253 <NA>        13     9     NA   -1.61         NA constant
      254          254 <NA>        13    10     NA   -1.61         NA constant
      255          255 <NA>        13    11     NA   -1.61         NA constant
      256          256 <NA>        13    12     NA   -1.61         NA constant
      257          257 <NA>        14     1     NA   -1.61         NA constant
      258          258 <NA>        14     2     NA   -1.61         NA constant
      259          259 <NA>        14     3     NA   -1.61         NA constant
      260          260 <NA>        14     4     NA   -1.61         NA constant
      261          261 <NA>        14     5     NA   -1.61         NA constant
      262          262 <NA>        14     6     NA   -1.61         NA constant
      263          263 <NA>        14     7     NA   -1.61         NA constant
      264          264 <NA>        14     8     NA   -1.61         NA constant
      265          265 <NA>        14     9     NA   -1.61         NA constant
      266          266 <NA>        14    10     NA   -1.61         NA constant
      267          267 <NA>        14    11     NA   -1.61         NA constant
      268          268 <NA>        14    12     NA   -1.61         NA constant
      269          269 <NA>        15     1     NA   -1.61         NA constant
      270          270 <NA>        15     2     NA   -1.61         NA constant
      271          271 <NA>        15     3     NA   -1.61         NA constant
      272          272 <NA>        15     4     NA   -1.61         NA constant
      273          273 <NA>        15     5     NA   -1.61         NA constant
      274          274 <NA>        15     6     NA   -1.61         NA constant
      275          275 <NA>        15     7     NA   -1.61         NA constant
      276          276 <NA>        15     8     NA   -1.61         NA constant
      277          277 <NA>        15     9     NA   -1.61         NA constant
      278          278 <NA>        15    10     NA   -1.61         NA constant
      279          279 <NA>        15    11     NA   -1.61         NA constant
      280          280 <NA>        15    12     NA   -1.61         NA constant
      281          281 <NA>        16     1     NA   -1.61         NA constant
      282          282 <NA>        16     2     NA   -1.61         NA constant
      283          283 <NA>        16     3     NA   -1.61         NA constant
      284          284 <NA>        16     4     NA   -1.61         NA constant
      285          285 <NA>        16     5     NA   -1.61         NA constant
      286          286 <NA>        16     6     NA   -1.61         NA constant
      287          287 <NA>        16     7     NA   -1.61         NA constant
      288          288 <NA>        16     8     NA   -1.61         NA constant
      289          289 <NA>        16     9     NA   -1.61         NA constant
      290          290 <NA>        16    10     NA   -1.61         NA constant
      291          291 <NA>        16    11     NA   -1.61         NA constant
      292          292 <NA>        16    12     NA   -1.61         NA constant
      293          293 <NA>        17     1     NA   -1.61         NA constant
      294          294 <NA>        17     2     NA   -1.61         NA constant
      295          295 <NA>        17     3     NA   -1.61         NA constant
      296          296 <NA>        17     4     NA   -1.61         NA constant
      297          297 <NA>        17     5     NA   -1.61         NA constant
      298          298 <NA>        17     6     NA   -1.61         NA constant
      299          299 <NA>        17     7     NA   -1.61         NA constant
      300          300 <NA>        17     8     NA   -1.61         NA constant
      301          301 <NA>        17     9     NA   -1.61         NA constant
      302          302 <NA>        17    10     NA   -1.61         NA constant
      303          303 <NA>        17    11     NA   -1.61         NA constant
      304          304 <NA>        17    12     NA   -1.61         NA constant
      305          305 <NA>        18     1     NA   -1.61         NA constant
      306          306 <NA>        18     2     NA   -1.61         NA constant
      307          307 <NA>        18     3     NA   -1.61         NA constant
      308          308 <NA>        18     4     NA   -1.61         NA constant
      309          309 <NA>        18     5     NA   -1.61         NA constant
      310          310 <NA>        18     6     NA   -1.61         NA constant
      311          311 <NA>        18     7     NA   -1.61         NA constant
      312          312 <NA>        18     8     NA   -1.61         NA constant
      313          313 <NA>        18     9     NA   -1.61         NA constant
      314          314 <NA>        18    10     NA   -1.61         NA constant
      315          315 <NA>        18    11     NA   -1.61         NA constant
      316          316 <NA>        18    12     NA   -1.61         NA constant
      317          317 <NA>        19     1     NA   -1.61         NA constant
      318          318 <NA>        19     2     NA   -1.61         NA constant
      319          319 <NA>        19     3     NA   -1.61         NA constant
      320          320 <NA>        19     4     NA   -1.61         NA constant
          distribution input_type  lpdf log_sd
          <chr>        <chr>      <dbl>  <dbl>
        1 <NA>         <NA>       NA     NA
        2 <NA>         <NA>       NA     NA
        3 <NA>         <NA>       NA     NA
        4 <NA>         <NA>       NA     NA
        5 <NA>         <NA>       NA     NA
        6 <NA>         <NA>       NA     NA
        7 <NA>         <NA>       NA     NA
        8 <NA>         <NA>       NA     NA
        9 <NA>         <NA>       NA     NA
       10 <NA>         <NA>       NA     NA
       11 <NA>         <NA>       NA     NA
       12 <NA>         <NA>       NA     NA
       13 <NA>         <NA>       NA     NA
       14 <NA>         <NA>       NA     NA
       15 <NA>         <NA>       NA     NA
       16 <NA>         <NA>       NA     NA
       17 <NA>         <NA>       NA     NA
       18 <NA>         <NA>       NA     NA
       19 <NA>         <NA>       NA     NA
       20 <NA>         <NA>       NA     NA
       21 <NA>         <NA>       NA     NA
       22 <NA>         <NA>       NA     NA
       23 <NA>         <NA>       NA     NA
       24 <NA>         <NA>       NA     NA
       25 <NA>         <NA>       NA     NA
       26 <NA>         <NA>       NA     NA
       27 <NA>         <NA>       NA     NA
       28 <NA>         <NA>       NA     NA
       29 <NA>         <NA>       NA     NA
       30 <NA>         <NA>       NA     NA
       31 <NA>         <NA>       NA     NA
       32 <NA>         <NA>       NA     NA
       33 <NA>         <NA>       NA     NA
       34 <NA>         <NA>       NA     NA
       35 <NA>         <NA>       NA     NA
       36 <NA>         <NA>       NA     NA
       37 <NA>         <NA>       NA     NA
       38 <NA>         <NA>       NA     NA
       39 <NA>         <NA>       NA     NA
       40 <NA>         <NA>       NA     NA
       41 <NA>         <NA>       NA     NA
       42 <NA>         <NA>       NA     NA
       43 <NA>         <NA>       NA     NA
       44 <NA>         <NA>       NA     NA
       45 <NA>         <NA>       NA     NA
       46 <NA>         <NA>       NA     NA
       47 <NA>         <NA>       NA     NA
       48 <NA>         <NA>       NA     NA
       49 <NA>         <NA>       NA     NA
       50 <NA>         <NA>       NA     NA
       51 <NA>         <NA>       NA     NA
       52 <NA>         <NA>       NA     NA
       53 <NA>         <NA>       NA     NA
       54 <NA>         <NA>       NA     NA
       55 <NA>         <NA>       NA     NA
       56 <NA>         <NA>       NA     NA
       57 <NA>         <NA>       NA     NA
       58 <NA>         <NA>       NA     NA
       59 <NA>         <NA>       NA     NA
       60 <NA>         <NA>       NA     NA
       61 <NA>         <NA>       NA     NA
       62 <NA>         <NA>       NA     NA
       63 <NA>         <NA>       NA     NA
       64 <NA>         <NA>       NA     NA
       65 <NA>         <NA>       NA     NA
       66 <NA>         <NA>       NA     NA
       67 <NA>         <NA>       NA     NA
       68 <NA>         <NA>       NA     NA
       69 <NA>         <NA>       -8.15  -1.13
       70 <NA>         <NA>       -8.15  -1.13
       71 <NA>         <NA>       -8.15  -1.13
       72 <NA>         <NA>       -8.15  -1.13
       73 <NA>         <NA>       -8.15  -1.13
       74 <NA>         <NA>       -8.15  -1.13
       75 <NA>         <NA>       -8.15  -1.13
       76 <NA>         <NA>       -8.15  -1.13
       77 <NA>         <NA>       -8.15  -1.13
       78 <NA>         <NA>       -8.15  -1.13
       79 <NA>         <NA>       -8.15  -1.13
       80 <NA>         <NA>       -8.15  -1.13
       81 <NA>         <NA>       -8.15  -1.13
       82 <NA>         <NA>       -8.15  -1.13
       83 <NA>         <NA>       -8.15  -1.13
       84 <NA>         <NA>       -8.15  -1.13
       85 <NA>         <NA>       -8.15  -1.13
       86 <NA>         <NA>       -8.15  -1.13
       87 <NA>         <NA>       -8.15  -1.13
       88 <NA>         <NA>       -8.15  -1.13
       89 <NA>         <NA>       -8.15  -1.13
       90 <NA>         <NA>       -8.15  -1.13
       91 <NA>         <NA>       -8.15  -1.13
       92 <NA>         <NA>       -8.15  -1.13
       93 <NA>         <NA>       -8.15  -1.13
       94 <NA>         <NA>       -8.15  -1.13
       95 <NA>         <NA>       -8.15  -1.13
       96 <NA>         <NA>       -8.15  -1.13
       97 <NA>         <NA>       -8.15  -1.13
       98 normal       <NA>       NA     NA
       99 <NA>         <NA>       NA     NA
      100 <NA>         <NA>       NA     NA
      101 <NA>         <NA>       NA     NA
      102 <NA>         <NA>       NA     NA
      103 <NA>         <NA>       NA     NA
      104 <NA>         <NA>       NA     NA
      105 <NA>         <NA>       NA     NA
      106 <NA>         <NA>       NA     NA
      107 <NA>         <NA>       NA     NA
      108 <NA>         <NA>       NA     NA
      109 <NA>         <NA>       NA     NA
      110 <NA>         <NA>       NA     NA
      111 <NA>         <NA>       NA     NA
      112 <NA>         <NA>       NA     NA
      113 <NA>         <NA>       NA     NA
      114 <NA>         <NA>       NA     NA
      115 <NA>         <NA>       NA     NA
      116 <NA>         <NA>       NA     NA
      117 <NA>         <NA>       NA     NA
      118 <NA>         <NA>       NA     NA
      119 <NA>         <NA>       NA     NA
      120 <NA>         <NA>       NA     NA
      121 <NA>         <NA>       NA     NA
      122 <NA>         <NA>       NA     NA
      123 <NA>         <NA>       NA     NA
      124 <NA>         <NA>       NA     NA
      125 <NA>         <NA>       NA     NA
      126 <NA>         <NA>       NA     NA
      127 <NA>         <NA>       NA     NA
      128 <NA>         <NA>       NA     NA
      129 <NA>         <NA>       NA     NA
      130 <NA>         <NA>       NA     NA
      131 <NA>         <NA>       NA     NA
      132 <NA>         <NA>       NA     NA
      133 <NA>         <NA>       NA     NA
      134 <NA>         <NA>       NA     NA
      135 <NA>         <NA>       NA     NA
      136 <NA>         <NA>       NA     NA
      137 <NA>         <NA>       NA     NA
      138 <NA>         <NA>       NA     NA
      139 <NA>         <NA>       NA     NA
      140 <NA>         <NA>       NA     NA
      141 <NA>         <NA>       NA     NA
      142 <NA>         <NA>       NA     NA
      143 <NA>         <NA>       NA     NA
      144 <NA>         <NA>       NA     NA
      145 <NA>         <NA>       NA     NA
      146 <NA>         <NA>       NA     NA
      147 <NA>         <NA>       NA     NA
      148 <NA>         <NA>       NA     NA
      149 <NA>         <NA>       NA     NA
      150 <NA>         <NA>       NA     NA
      151 <NA>         <NA>       NA     NA
      152 <NA>         <NA>       NA     NA
      153 <NA>         <NA>       NA     NA
      154 <NA>         <NA>       NA     NA
      155 <NA>         <NA>       NA     NA
      156 <NA>         <NA>       NA     NA
      157 <NA>         <NA>       NA     NA
      158 <NA>         <NA>       NA     NA
      159 <NA>         <NA>       NA     NA
      160 <NA>         <NA>       NA     NA
      161 <NA>         <NA>       NA     NA
      162 <NA>         <NA>       NA     NA
      163 <NA>         <NA>       NA     NA
      164 <NA>         <NA>       NA     NA
      165 <NA>         <NA>       NA     NA
      166 <NA>         <NA>       NA     NA
      167 <NA>         <NA>       NA     NA
      168 <NA>         <NA>       NA     NA
      169 <NA>         <NA>       NA     NA
      170 <NA>         <NA>       NA     NA
      171 <NA>         <NA>       NA     NA
      172 <NA>         <NA>       NA     NA
      173 <NA>         <NA>       NA     NA
      174 <NA>         <NA>       NA     NA
      175 <NA>         <NA>       NA     NA
      176 <NA>         <NA>       NA     NA
      177 <NA>         <NA>       NA     NA
      178 <NA>         <NA>       NA     NA
      179 <NA>         <NA>       NA     NA
      180 <NA>         <NA>       NA     NA
      181 <NA>         <NA>       NA     NA
      182 <NA>         <NA>       NA     NA
      183 <NA>         <NA>       NA     NA
      184 <NA>         <NA>       NA     NA
      185 <NA>         <NA>       NA     NA
      186 <NA>         <NA>       NA     NA
      187 <NA>         <NA>       NA     NA
      188 <NA>         <NA>       NA     NA
      189 <NA>         <NA>       NA     NA
      190 <NA>         <NA>       NA     NA
      191 <NA>         <NA>       NA     NA
      192 <NA>         <NA>       NA     NA
      193 <NA>         <NA>       NA     NA
      194 <NA>         <NA>       NA     NA
      195 <NA>         <NA>       NA     NA
      196 <NA>         <NA>       NA     NA
      197 <NA>         <NA>       NA     NA
      198 <NA>         <NA>       NA     NA
      199 <NA>         <NA>       NA     NA
      200 <NA>         <NA>       NA     NA
      201 <NA>         <NA>       NA     NA
      202 <NA>         <NA>       NA     NA
      203 <NA>         <NA>       NA     NA
      204 <NA>         <NA>       NA     NA
      205 <NA>         <NA>       NA     NA
      206 <NA>         <NA>       NA     NA
      207 <NA>         <NA>       NA     NA
      208 <NA>         <NA>       NA     NA
      209 <NA>         <NA>       NA     NA
      210 <NA>         <NA>       NA     NA
      211 <NA>         <NA>       NA     NA
      212 <NA>         <NA>       NA     NA
      213 <NA>         <NA>       NA     NA
      214 <NA>         <NA>       NA     NA
      215 <NA>         <NA>       NA     NA
      216 <NA>         <NA>       NA     NA
      217 <NA>         <NA>       NA     NA
      218 <NA>         <NA>       NA     NA
      219 <NA>         <NA>       NA     NA
      220 <NA>         <NA>       NA     NA
      221 <NA>         <NA>       NA     NA
      222 <NA>         <NA>       NA     NA
      223 <NA>         <NA>       NA     NA
      224 <NA>         <NA>       NA     NA
      225 <NA>         <NA>       NA     NA
      226 <NA>         <NA>       NA     NA
      227 <NA>         <NA>       NA     NA
      228 <NA>         <NA>       NA     NA
      229 <NA>         <NA>       NA     NA
      230 <NA>         <NA>       NA     NA
      231 <NA>         <NA>       NA     NA
      232 <NA>         <NA>       NA     NA
      233 <NA>         <NA>       NA     NA
      234 <NA>         <NA>       NA     NA
      235 <NA>         <NA>       NA     NA
      236 <NA>         <NA>       NA     NA
      237 <NA>         <NA>       NA     NA
      238 <NA>         <NA>       NA     NA
      239 <NA>         <NA>       NA     NA
      240 <NA>         <NA>       NA     NA
      241 <NA>         <NA>       NA     NA
      242 <NA>         <NA>       NA     NA
      243 <NA>         <NA>       NA     NA
      244 <NA>         <NA>       NA     NA
      245 <NA>         <NA>       NA     NA
      246 <NA>         <NA>       NA     NA
      247 <NA>         <NA>       NA     NA
      248 <NA>         <NA>       NA     NA
      249 <NA>         <NA>       NA     NA
      250 <NA>         <NA>       NA     NA
      251 <NA>         <NA>       NA     NA
      252 <NA>         <NA>       NA     NA
      253 <NA>         <NA>       NA     NA
      254 <NA>         <NA>       NA     NA
      255 <NA>         <NA>       NA     NA
      256 <NA>         <NA>       NA     NA
      257 <NA>         <NA>       NA     NA
      258 <NA>         <NA>       NA     NA
      259 <NA>         <NA>       NA     NA
      260 <NA>         <NA>       NA     NA
      261 <NA>         <NA>       NA     NA
      262 <NA>         <NA>       NA     NA
      263 <NA>         <NA>       NA     NA
      264 <NA>         <NA>       NA     NA
      265 <NA>         <NA>       NA     NA
      266 <NA>         <NA>       NA     NA
      267 <NA>         <NA>       NA     NA
      268 <NA>         <NA>       NA     NA
      269 <NA>         <NA>       NA     NA
      270 <NA>         <NA>       NA     NA
      271 <NA>         <NA>       NA     NA
      272 <NA>         <NA>       NA     NA
      273 <NA>         <NA>       NA     NA
      274 <NA>         <NA>       NA     NA
      275 <NA>         <NA>       NA     NA
      276 <NA>         <NA>       NA     NA
      277 <NA>         <NA>       NA     NA
      278 <NA>         <NA>       NA     NA
      279 <NA>         <NA>       NA     NA
      280 <NA>         <NA>       NA     NA
      281 <NA>         <NA>       NA     NA
      282 <NA>         <NA>       NA     NA
      283 <NA>         <NA>       NA     NA
      284 <NA>         <NA>       NA     NA
      285 <NA>         <NA>       NA     NA
      286 <NA>         <NA>       NA     NA
      287 <NA>         <NA>       NA     NA
      288 <NA>         <NA>       NA     NA
      289 <NA>         <NA>       NA     NA
      290 <NA>         <NA>       NA     NA
      291 <NA>         <NA>       NA     NA
      292 <NA>         <NA>       NA     NA
      293 <NA>         <NA>       NA     NA
      294 <NA>         <NA>       NA     NA
      295 <NA>         <NA>       NA     NA
      296 <NA>         <NA>       NA     NA
      297 <NA>         <NA>       NA     NA
      298 <NA>         <NA>       NA     NA
      299 <NA>         <NA>       NA     NA
      300 <NA>         <NA>       NA     NA
      301 <NA>         <NA>       NA     NA
      302 <NA>         <NA>       NA     NA
      303 <NA>         <NA>       NA     NA
      304 <NA>         <NA>       NA     NA
      305 <NA>         <NA>       NA     NA
      306 <NA>         <NA>       NA     NA
      307 <NA>         <NA>       NA     NA
      308 <NA>         <NA>       NA     NA
      309 <NA>         <NA>       NA     NA
      310 <NA>         <NA>       NA     NA
      311 <NA>         <NA>       NA     NA
      312 <NA>         <NA>       NA     NA
      313 <NA>         <NA>       NA     NA
      314 <NA>         <NA>       NA     NA
      315 <NA>         <NA>       NA     NA
      316 <NA>         <NA>       NA     NA
      317 <NA>         <NA>       NA     NA
      318 <NA>         <NA>       NA     NA
      319 <NA>         <NA>       NA     NA
      320 <NA>         <NA>       NA     NA
      # i 13,647 more rows

