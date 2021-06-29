# bt_test_mf works

    Code
      bt_test_mf(cs1, cs2, subj = subj, data = example_data)
    Message <simpleMessage>
      t is large; approximation invoked.
      t is large; approximation invoked.
      t is large; approximation invoked.
      t is large; approximation invoked.
    Output
      # A tibble: 2 x 16
        x     y     exclusion cut_off model controls method p.value effect.size
        <chr> <chr> <chr>     <chr>   <chr> <lgl>    <chr>  <lgl>   <lgl>      
      1 cs    scr   full data full d~ Baye~ NA       Bayes~ NA      NA         
      2 cs    scr   full data full d~ Baye~ NA       Bayes~ NA      NA         
      # ... with 7 more variables: effect.size.ma <lgl>, estimate <dbl>,
      #   statistic <lgl>, conf.low <lgl>, conf.high <lgl>, framework <chr>,
      #   data_used <list>

# bt_test_mf for groups works

    Code
      bt_test_mf(cs1, cs2, subj = subj, group = group, data = example_data)
    Message <simpleMessage>
      t is large; approximation invoked.
      t is large; approximation invoked.
      t is large; approximation invoked.
      t is large; approximation invoked.
    Output
      # A tibble: 2 x 16
        x     y     exclusion cut_off model controls method p.value effect.size
        <chr> <chr> <chr>     <chr>   <chr> <lgl>    <chr>  <lgl>   <lgl>      
      1 cs    scr   full data full d~ Baye~ NA       Bayes~ NA      NA         
      2 cs    scr   full data full d~ Baye~ NA       Bayes~ NA      NA         
      # ... with 7 more variables: effect.size.ma <lgl>, estimate <dbl>,
      #   statistic <lgl>, conf.low <lgl>, conf.high <lgl>, framework <chr>,
      #   data_used <list>

# t_test_mf works

    Code
      t_test_mf(cs1, cs2, subj = subj, data = example_data)
    Output
      # A tibble: 2 x 18
        x     y     exclusion cut_off model controls method p.value effect.size
        <chr> <chr> <chr>     <chr>   <chr> <lgl>    <chr>    <dbl>       <dbl>
      1 cs    scr   full data full d~ t-te~ NA       t-tes~ 1.24e-7       0.455
      2 cs    scr   full data full d~ t-te~ NA       t-tes~ 2.47e-7       0.455
      # ... with 9 more variables: effect.size.ma <dbl>, effect.size.ma.lci <dbl>,
      #   effect.size.ma.hci <dbl>, estimate <dbl>, statistic <dbl>, conf.low <dbl>,
      #   conf.high <dbl>, framework <chr>, data_used <list>

# t_test_mf for groups works

    Code
      t_test_mf(cs1 = cs1, cs2 = cs2, subj = subj, group = group, data = example_data)
    Output
      # A tibble: 2 x 18
        x     y     exclusion cut_off model controls method p.value effect.size
        <chr> <chr> <chr>     <chr>   <chr> <lgl>    <chr>    <dbl>       <dbl>
      1 cs    scr   full data full d~ t-te~ NA       t-tes~   0.624      -0.102
      2 cs    scr   full data full d~ t-te~ NA       t-tes~   0.752      -0.102
      # ... with 9 more variables: effect.size.ma <dbl>, effect.size.ma.lci <dbl>,
      #   effect.size.ma.hci <dbl>, estimate <dbl>, statistic <dbl>, conf.low <dbl>,
      #   conf.high <dbl>, framework <chr>, data_used <list>

# rm_anova_mf works

    Code
      rm_anova_mf(cs1, cs2, subj = subj, data = example_data, time = FALSE, group = NULL,
        between = NULL)
    Output
      # A tibble: 1 x 18
        x     y     exclusion cut_off model controls method p.value effect.size
        <chr> <chr> <chr>     <chr>   <chr> <lgl>    <chr>    <dbl>       <dbl>
      1 cs    scr   full data full d~ rep ~ NA       rep A~ 2.47e-7      0.0847
      # ... with 9 more variables: effect.size.ma <dbl>, effect.size.ma.lci <dbl>,
      #   effect.size.ma.hci <dbl>, estimate <lgl>, statistic <dbl>, conf.low <lgl>,
      #   conf.high <lgl>, framework <chr>, data_used <list>

# rm_anova_mf 3 stimuli works

    Code
      rm_anova_mf(cs1, cs2, cs3, subj = subj, data = example_data, time = FALSE,
        group = NULL, between = NULL)
    Warning <simpleWarning>
      You have selected 3 CSs. At the moment the package does not support
                  the meta-analytic effect size for more than 2 stimuli. The reported meta-analytic effect
                  size for the ANOVA corresponds to only 'cs1' and 'cs2', and the 'cs3' is not taken into account.
    Output
      # A tibble: 1 x 18
        x     y     exclusion cut_off model controls method p.value effect.size
        <chr> <chr> <chr>     <chr>   <chr> <lgl>    <chr>    <dbl>       <dbl>
      1 cs    scr   full data full d~ rep ~ NA       rep A~ 2.47e-7      0.0847
      # ... with 9 more variables: effect.size.ma <dbl>, effect.size.ma.lci <dbl>,
      #   effect.size.ma.hci <dbl>, estimate <lgl>, statistic <dbl>, conf.low <lgl>,
      #   conf.high <lgl>, framework <chr>, data_used <list>

# rm_anova_mf for groups works

    Code
      rm_anova_mf(cs1, cs2, subj = subj, group = group, data = example_data)
    Output
      # A tibble: 1 x 18
        x     y     exclusion cut_off model controls method p.value effect.size
        <chr> <chr> <chr>     <chr>   <chr> <lgl>    <chr>    <dbl>       <dbl>
      1 grou~ scr   full data full d~ rep ~ NA       rep A~   0.205     0.00227
      # ... with 9 more variables: effect.size.ma <dbl>, effect.size.ma.lci <dbl>,
      #   effect.size.ma.hci <dbl>, estimate <lgl>, statistic <dbl>, conf.low <lgl>,
      #   conf.high <lgl>, framework <chr>, data_used <list>

# rm_anova_mf 3 stimuli works and group

    Code
      rm_anova_mf(cs1, cs2, cs3, subj = subj, data = example_data, time = FALSE,
        group = group, between = NULL)
    Warning <simpleWarning>
      You have selected 3 CSs. At the moment the package does not support
                  the meta-analytic effect size for more than 2 stimuli. The reported meta-analytic effect
                  size for the ANOVA corresponds to only 'cs1' and 'cs2', and the 'cs3' is not taken into account.
    Output
      # A tibble: 1 x 18
        x     y     exclusion cut_off model controls method p.value effect.size
        <chr> <chr> <chr>     <chr>   <chr> <lgl>    <chr>    <dbl>       <dbl>
      1 grou~ scr   full data full d~ rep ~ NA       rep A~   0.752    -0.00230
      # ... with 9 more variables: effect.size.ma <dbl>, effect.size.ma.lci <dbl>,
      #   effect.size.ma.hci <dbl>, estimate <lgl>, statistic <dbl>, conf.low <lgl>,
      #   conf.high <lgl>, framework <chr>, data_used <list>

# rm_banova_mf works

    Code
      rm_banova_mf(cs1, cs2, subj = subj, data = example_data)
    Output
      # A tibble: 1 x 18
        x     y     exclusion cut_off model controls method p.value effect.size
        <chr> <chr> <chr>     <chr>   <chr> <lgl>    <chr>  <lgl>   <lgl>      
      1 cs:t~ scr   full data full d~ rep ~ NA       rep B~ NA      NA         
      # ... with 9 more variables: effect.size.ma <lgl>, effect.size.ma.lci <lgl>,
      #   effect.size.ma.hci <lgl>, estimate <dbl>, statistic <lgl>, conf.low <lgl>,
      #   conf.high <lgl>, framework <chr>, data_used <list>

# rm_banova_mf for groups works

    Code
      rm_banova_mf(cs1, cs2, subj = subj, group = group, data = example_data)
    Output
      # A tibble: 1 x 18
        x     y     exclusion cut_off model controls method p.value effect.size
        <chr> <chr> <chr>     <chr>   <chr> <lgl>    <chr>  <lgl>   <lgl>      
      1 cs:g~ scr   full data full d~ rep ~ NA       rep B~ NA      NA         
      # ... with 9 more variables: effect.size.ma <lgl>, effect.size.ma.lci <lgl>,
      #   effect.size.ma.hci <lgl>, estimate <dbl>, statistic <lgl>, conf.low <lgl>,
      #   conf.high <lgl>, framework <chr>, data_used <list>

# chop cs works

    Code
      chop_cs(cs = cs1, data = bf_data, subj = subj)
    Message <message>
      Note: Using an external vector in selections is ambiguous.
      i Use `all_of(index)` instead of `index` to silence this message.
      i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      This message is displayed once per session.
      Note: Using an external vector in selections is ambiguous.
      i Use `all_of(x)` instead of `x` to silence this message.
      i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
      This message is displayed once per session.
    Output
      # A tibble: 8 x 32
           id cs_t_1 cs_t_2 cs_t_3 cs_t_4 cs_t_5 cs_t_6 cs_t_7 cs_t_8 cs_t_9 cs_t_10
        <int>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>   <dbl>
      1     2 0.629  0.930   1.04   0.933  0.974  0.886  0.842  0.982  0.579   0.382
      2     3 7.08   4.98   12.8    8.28   4.98   8.87   8.25   0.761  1.77    2.18 
      3     4 0      1.22    2.44   4.51   5.34   4.68   3.65   2.10   6.48    2.64 
      4     5 1.44   0.971   0.378  1.44   0.406  1.60   1.53   1.19   0.399   0.617
      5     7 0.945  3.29    3.44   2.71   3.64   3.89   3.62   2.29   5.28    4.52 
      6     8 3.24   2.34    2.39   2.29   1.96   2.51   2.42   2.72   3.75    0.551
      7     9 0.438  0.0467  0.231  2.24   1.62   1.86   1.74   1.60   0.873   2.55 
      8    10 0.0391 0.208   0.465  0.540  0.450  0.552  0.414  0.318  0.456   0.343
      # ... with 21 more variables: cs_t10per <dbl>, cs_b10per <dbl>,
      #   cs_minfirst <dbl>, cs_all <dbl>, cs_t33per <dbl>, cs_m33per <dbl>,
      #   cs_b33per <dbl>, cs_fhalf <dbl>, cs_lhalf <dbl>, cs_ftrial <dbl>,
      #   cs_ltrial <dbl>, cs_t20per <dbl>, cs_b20per <dbl>, cs_f2trial <dbl>,
      #   cs_l2trial <dbl>, cs_per2_1 <dbl>, cs_per2_2 <dbl>, cs_per2_3 <dbl>,
      #   cs_per2_4 <dbl>, cs_per2_5 <dbl>, cs_l5trial <dbl>

# chop cs for groups works

    Code
      chop_cs(cs = cs1, data = bf_data, subj = subj, group = group)
    Output
      # A tibble: 8 x 32
           id cs_t_1 cs_t_2 cs_t_3 cs_t_4 cs_t_5 cs_t_6 cs_t_7 cs_t_8 cs_t_9 cs_t_10
        <int>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>   <dbl>
      1     2 0.629  0.930   1.04   0.933  0.974  0.886  0.842  0.982  0.579   0.382
      2     3 7.08   4.98   12.8    8.28   4.98   8.87   8.25   0.761  1.77    2.18 
      3     4 0      1.22    2.44   4.51   5.34   4.68   3.65   2.10   6.48    2.64 
      4     5 1.44   0.971   0.378  1.44   0.406  1.60   1.53   1.19   0.399   0.617
      5     7 0.945  3.29    3.44   2.71   3.64   3.89   3.62   2.29   5.28    4.52 
      6     8 3.24   2.34    2.39   2.29   1.96   2.51   2.42   2.72   3.75    0.551
      7     9 0.438  0.0467  0.231  2.24   1.62   1.86   1.74   1.60   0.873   2.55 
      8    10 0.0391 0.208   0.465  0.540  0.450  0.552  0.414  0.318  0.456   0.343
      # ... with 21 more variables: cs_t10per <dbl>, cs_b10per <dbl>,
      #   cs_minfirst <dbl>, cs_all <dbl>, cs_t33per <dbl>, cs_m33per <dbl>,
      #   cs_b33per <dbl>, cs_fhalf <dbl>, cs_lhalf <dbl>, cs_ftrial <dbl>,
      #   cs_ltrial <dbl>, cs_t20per <dbl>, cs_b20per <dbl>, cs_f2trial <dbl>,
      #   cs_l2trial <dbl>, cs_per2_1 <dbl>, cs_per2_2 <dbl>, cs_per2_3 <dbl>,
      #   cs_per2_4 <dbl>, cs_per2_5 <dbl>, cs_l5trial <dbl>

# chop css works

    Code
      chop_css(cs1 = cs1, cs2 = cs2, data = bf_data, subj = subj, group = group)
    Output
        id    cs1_t_1    cs1_t_2    cs1_t_3   cs1_t_4   cs1_t_5   cs1_t_6   cs1_t_7
      1  2 0.62905570 0.92988220  1.0415000 0.9330014 0.9743682 0.8855119 0.8418664
      2  3 7.07953400 4.97656500 12.7791100 8.2847610 4.9798520 8.8685450 8.2468480
      3  4 0.00000000 1.21569500  2.4367400 4.5100300 5.3372170 4.6831920 3.6484020
      4  5 1.43786800 0.97055850  0.3784756 1.4434960 0.4060977 1.6015840 1.5274500
      5  7 0.94540620 3.29359400  3.4376340 2.7078930 3.6439630 3.8878390 3.6174920
      6  8 3.23735900 2.34393600  2.3931960 2.2908990 1.9583950 2.5094320 2.4234050
      7  9 0.43832140 0.04669674  0.2307273 2.2448410 1.6178050 1.8630760 1.7384420
      8 10 0.03912846 0.20814220  0.4654428 0.5395534 0.4498870 0.5519411 0.4144870
          cs1_t_8   cs1_t_9  cs1_t_10 cs1_t10per cs1_b10per cs1_minfirst   cs1_all
      1 0.9821523 0.5790559 0.3824215 0.62905570  0.4807387    0.8388622 0.8178815
      2 0.7607041 1.7655060 2.1758810 7.07953400  1.9706935    5.8708636 5.9917306
      3 2.1018580 6.4756940 2.6422620 0.00000000  4.5589780    3.6723433 3.3051090
      4 1.1900760 0.3994079 0.6168466 1.43786800  0.5081273    0.9482214 0.9971860
      5 2.2854040 5.2819210 4.5153330 0.94540620  4.8986270    3.6301192 3.3616479
      6 2.7189870 3.7545760 0.5508313 3.23735900  2.1527037    2.3270730 2.4181016
      7 1.5976320 0.8726718 2.5473010 0.43832140  1.7099864    1.4176881 1.3197514
      8 0.3175979 0.4557406 0.3433352 0.03912846  0.3995379    0.4162364 0.3785256
        cs1_t33per cs1_m33per cs1_b33per cs1_fhalf cs1_lhalf cs1_ftrial cs1_ltrial
      1  0.8668126  0.8910652  0.6963740 0.9015615 0.7342016 0.62905570  0.3824215
      2  8.2784030  6.5264752  3.2372348 7.6199644 4.3634968 7.07953400  2.1758810
      3  1.2174783  4.1704476  3.7170540 2.6999364 3.9102816 0.00000000  2.6422620
      4  0.9289674  0.9923696  0.9334451 0.9272992 1.0670729 1.43786800  0.6168466
      5  2.5588781  3.5517351  3.9250375 2.8056980 3.9175978 0.94540620  4.5153330
      6  2.6581637  2.5784129  2.3619498 2.4447570 2.3914463 3.23735900  0.5508313
      7  0.2385818  1.4521707  1.6890117 0.9156783 1.7238246 0.43832140  2.5473010
      8  0.2375712  0.4563785  0.3827902 0.3404308 0.4166204 0.03912846  0.3433352
        cs1_t20per cs1_b20per cs1_f2trial cs1_l2trial cs1_per2_1 cs1_per2_2
      1  0.7794690  0.6478766   0.7794690   0.4807387  0.7794690  0.9872507
      2  6.0280495  1.5673637   6.0280495   1.9706935  6.0280495 10.5319355
      3  0.6078475  3.7399380   0.6078475   4.5589780  0.6078475  3.4733850
      4  1.2042133  0.7354435   1.2042133   0.5081273  1.2042133  0.9109858
      5  2.1195001  4.0275527   2.1195001   4.8986270  2.1195001  3.0727635
      6  2.7906475  2.3414648   2.7906475   2.1527037  2.7906475  2.3420475
      7  0.2425091  1.6725349   0.2425091   1.7099864  0.2425091  1.2377841
      8  0.1236353  0.3722246   0.1236353   0.3995379  0.1236353  0.5024981
        cs1_per2_3 cs1_per2_4 cs1_per2_5 cs1_l5trial    cs2_t_1   cs2_t_2   cs2_t_3
      1  0.9299401  0.9120093  0.4807387   0.7742294  0.9857365 1.2673990 0.0000000
      2  6.9241985  4.5037760  1.9706935   4.4662227 13.6548600 0.9066070 0.0000000
      3  5.0102045  2.8751300  4.5589780   4.1481042  0.0000000 3.2380020 2.4289650
      4  1.0038409  1.3587630  0.5081273   0.9569104  1.3146440 0.0000000 0.6570014
      5  3.7659010  2.9514480  4.8986270   3.8719920  4.2762160 3.1962110 3.4280200
      6  2.2339135  2.5711960  2.1527037   2.3192710  2.7552620 2.2843320 0.1147742
      7  1.7404405  1.6680370  1.7099864   1.7061546  0.3066088 1.5306480 2.1478420
      8  0.5009140  0.3660425  0.3995379   0.4221648  0.2956061 0.1793028 0.2681102
           cs2_t_4    cs2_t_5    cs2_t_6   cs2_t_7    cs2_t_8    cs2_t_9   cs2_t_10
      1 0.64031250 0.05137117 0.03480838 0.0000000 0.00000000 0.29036070 0.31496240
      2 2.91306300 0.60658980 3.72999700 1.1069910 3.63702500 0.00000000 0.00000000
      3 0.25262940 0.00000000 2.51429900 0.0000000 0.18621060 0.52076760 0.95751230
      4 1.37108400 0.53106310 0.54313130 0.2875019 0.00000000 0.18795960 0.18725370
      5 1.88772700 3.06479700 2.91668000 2.7914400 1.22984300 3.37716300 2.70570100
      6 0.42298280 2.22446600 0.00000000 0.0000000 1.21598600 0.00000000 0.00000000
      7 0.46613780 0.16134580 0.91540300 0.0000000 0.30176180 0.08460228 0.09295772
      8 0.05366129 0.10903030 0.00000000 0.0000000 0.04329369 0.19495150 0.30379580
        cs2_t10per cs2_b10per cs2_minfirst   cs2_all cs2_t33per cs2_m33per cs2_b33per
      1  0.9857365  0.3026616    0.2888016 0.3584951  0.7510452 0.14526468  0.1513308
      2 13.6548600  0.0000000    1.4333636 2.6555133  4.8538223 1.71338083  1.1860040
      3  0.0000000  0.7391399    1.1220429 1.0098386  1.8889890 0.84326737  0.4161226
      4  1.3146440  0.1876066    0.4183328 0.5079639  0.6572151 0.51110590  0.1656788
      5  4.2762160  3.0414320    2.7330647 2.8873798  3.6334823 2.67081000  2.5260367
      6  2.7552620  0.0000000    0.6958379 0.9017803  1.7181227 0.56831557  0.3039965
      7  0.3066088  0.0887800    0.6334109 0.6007307  1.3283663 0.58244181  0.1198305
      8  0.2956061  0.2493737    0.1280162 0.1447752  0.2476730 0.09557814  0.1355102
        cs2_fhalf cs2_lhalf cs2_ftrial cs2_ltrial cs2_t20per cs2_b20per cs2_f2trial
      1 0.5889638 0.1280263  0.9857365 0.31496240  1.1265677  0.2017744   1.1265677
      2 3.6162240 1.6948026 13.6548600 0.00000000  7.2807335  1.2123417   7.2807335
      3 1.1839193 0.8357579  0.0000000 0.95751230  1.6190010  0.5548302   1.6190010
      4 0.7747585 0.2411693  1.3146440 0.18725370  0.6573220  0.1250711   0.6573220
      5 3.1705942 2.6041654  4.2762160 2.70570100  3.7362135  2.4375690   3.7362135
      6 1.5603634 0.2431972  2.7552620 0.00000000  2.5197970  0.4053287   2.5197970
      7 0.9225165 0.2789450  0.3066088 0.09295772  0.9186284  0.1597739   0.9186284
      8 0.1811421 0.1084082  0.2956061 0.30379580  0.2374544  0.1806803   0.2374544
        cs2_l2trial cs2_per2_1 cs2_per2_2 cs2_per2_3 cs2_per2_4 cs2_per2_5
      1   0.3026616  1.1265677  0.3201562 0.04308977 0.00000000  0.3026616
      2   0.0000000  7.2807335  1.4565315 2.16829340 2.37200800  0.0000000
      3   0.7391399  1.6190010  1.3407972 1.25714950 0.09310530  0.7391399
      4   0.1876066  0.6573220  1.0140427 0.53709720 0.14375095  0.1876066
      5   3.0414320  3.7362135  2.6578735 2.99073850 2.01064150  3.0414320
      6   0.0000000  2.5197970  0.2688785 1.11223300 0.60799300  0.0000000
      7   0.0887800  0.9186284  1.3069899 0.53837440 0.15088090  0.0887800
      8   0.2493737  0.2374544  0.1608857 0.05451515 0.02164685  0.2493737
        cs2_l5trial group
      1   0.1152504     1
      2   1.5134338     2
      3   0.6964649     1
      4   0.2894849     2
      5   2.6809373     1
      6   0.5734087     2
      7   0.2593451     1
      8   0.1085119     2

# combine cs works

    Code
      combine_cs(cs1 = cs1, cs2 = cs2, data = bf_data)
    Output
      # A tibble: 8 x 35
           id   CSP1   CSP2   CSP3  CSP4  CSP5  CSP6  CSP7  CSP8  CSP9 CSP10   CSM1
        <int>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
      1     2 0.629  0.930   1.04  0.933 0.974 0.886 0.842 0.982 0.579 0.382  0.986
      2     3 7.08   4.98   12.8   8.28  4.98  8.87  8.25  0.761 1.77  2.18  13.7  
      3     4 0      1.22    2.44  4.51  5.34  4.68  3.65  2.10  6.48  2.64   0    
      4     5 1.44   0.971   0.378 1.44  0.406 1.60  1.53  1.19  0.399 0.617  1.31 
      5     7 0.945  3.29    3.44  2.71  3.64  3.89  3.62  2.29  5.28  4.52   4.28 
      6     8 3.24   2.34    2.39  2.29  1.96  2.51  2.42  2.72  3.75  0.551  2.76 
      7     9 0.438  0.0467  0.231 2.24  1.62  1.86  1.74  1.60  0.873 2.55   0.307
      8    10 0.0391 0.208   0.465 0.540 0.450 0.552 0.414 0.318 0.456 0.343  0.296
      # ... with 23 more variables: CSM2 <dbl>, CSM3 <dbl>, CSM4 <dbl>, CSM5 <dbl>,
      #   CSM6 <dbl>, CSM7 <dbl>, CSM8 <dbl>, CSM9 <dbl>, CSM10 <dbl>, CSU1 <dbl>,
      #   CSU2 <dbl>, CSU3 <dbl>, CSU4 <dbl>, CSU5 <dbl>, CSU6 <dbl>, CSU7 <dbl>,
      #   CSU8 <dbl>, CSU9 <dbl>, CSU10 <dbl>, counterbalancing <chr>, group <dbl>,
      #   cs1_mean <dbl>, cs2_mean <dbl>

# universe works

    Code
      universe_cs(cs1, cs2, subj = subj, data = example_data, include_bayes = FALSE)
    Output
      # A tibble: 4 x 18
        x     y     exclusion cut_off model controls method p.value effect.size
        <chr> <chr> <chr>     <chr>   <chr> <lgl>    <chr>    <dbl>       <dbl>
      1 cs    scr   full data full d~ t-te~ NA       t-tes~ 1.24e-7      0.455 
      2 cs    scr   full data full d~ t-te~ NA       t-tes~ 2.47e-7      0.455 
      3 cs:t~ scr   full data full d~ rep ~ NA       rep A~ 3.70e-9      0.0354
      4 cs    scr   full data full d~ rep ~ NA       rep A~ 2.47e-7      0.0847
      # ... with 9 more variables: effect.size.ma <dbl>, effect.size.ma.lci <dbl>,
      #   effect.size.ma.hci <dbl>, estimate <dbl>, statistic <dbl>, conf.low <dbl>,
      #   conf.high <dbl>, framework <chr>, data_used <list>

# universe works mixed

    Code
      universe_cs(cs1, cs2, subj = subj, data = example_data, include_bayes = FALSE,
        include_mixed = TRUE)
    Output
      # A tibble: 12 x 19
         x     y     exclusion cut_off model controls method  p.value effect.size
         <chr> <chr> <chr>     <chr>   <chr> <lgl>    <chr>     <dbl>       <dbl>
       1 cs    scr   full data full d~ t-te~ NA       t-tes~ 1.24e- 7      0.455 
       2 cs    scr   full data full d~ t-te~ NA       t-tes~ 2.47e- 7      0.455 
       3 cs:t~ scr   full data full d~ rep ~ NA       rep A~ 3.70e- 9      0.0354
       4 cs    scr   full data full d~ rep ~ NA       rep A~ 2.47e- 7      0.0847
       5 cscs2 scr   full data <NA>    mixe~ NA       mixed~ 1.06e-11     NA     
       6 cscs~ scr   full data <NA>    mixe~ NA       mixed~ 5.93e- 7     NA     
       7 cscs2 scr   full data <NA>    mixe~ NA       mixed~ 2.61e-22     NA     
       8 cscs~ scr   full data <NA>    mixe~ NA       mixed~ 3.36e-12     NA     
       9 cscs2 scr   full data <NA>    mixe~ NA       mixed~ 1.22e-12     NA     
      10 cscs~ scr   full data <NA>    mixe~ NA       mixed~ 1.69e- 7     NA     
      11 cscs2 scr   full data <NA>    mixe~ NA       mixed~ 2.61e-22     NA     
      12 cscs~ scr   full data <NA>    mixe~ NA       mixed~ 3.36e-12     NA     
      # ... with 10 more variables: effect.size.ma <dbl>, effect.size.ma.lci <dbl>,
      #   effect.size.ma.hci <dbl>, estimate <dbl>, statistic <dbl>, conf.low <dbl>,
      #   conf.high <dbl>, framework <chr>, data_used <list>, efffect.size.ma <dbl>

# universe works with groups

    Code
      universe_cs(cs1, cs2, subj = subj, data = example_data, group = group,
        include_bayes = FALSE)
    Output
      # A tibble: 4 x 18
        x     y     exclusion cut_off model controls method p.value effect.size
        <chr> <chr> <chr>     <chr>   <chr> <lgl>    <chr>    <dbl>       <dbl>
      1 cs    scr   full data full d~ t-te~ NA       t-tes~ 0.00377     0.455  
      2 cs    scr   full data full d~ t-te~ NA       t-tes~ 0.00753     0.455  
      3 grou~ scr   full data full d~ rep ~ NA       rep A~ 0.205       0.00227
      4 grou~ scr   full data full d~ rep ~ NA       rep A~ 0.752      -0.00230
      # ... with 9 more variables: effect.size.ma <dbl>, effect.size.ma.lci <dbl>,
      #   effect.size.ma.hci <dbl>, estimate <dbl>, statistic <dbl>, conf.low <dbl>,
      #   conf.high <dbl>, framework <chr>, data_used <list>

# multiverse works

    Code
      multiverse_cs(cs1, cs2, subj = subj, data = example_data, include_bayes = FALSE)
    Warning <simpleWarning>
      You have selected 3 CSs. At the moment the package does not support
                  the meta-analytic effect size for more than 2 stimuli. The reported meta-analytic effect
                  size for the ANOVA corresponds to only 'cs1' and 'cs2', and the 'cs3' is not taken into account.
      You have selected 3 CSs. At the moment the package does not support
                  the meta-analytic effect size for more than 2 stimuli. The reported meta-analytic effect
                  size for the ANOVA corresponds to only 'cs1' and 'cs2', and the 'cs3' is not taken into account.
      You have selected 3 CSs. At the moment the package does not support
                  the meta-analytic effect size for more than 2 stimuli. The reported meta-analytic effect
                  size for the ANOVA corresponds to only 'cs1' and 'cs2', and the 'cs3' is not taken into account.
      You have selected 3 CSs. At the moment the package does not support
                  the meta-analytic effect size for more than 2 stimuli. The reported meta-analytic effect
                  size for the ANOVA corresponds to only 'cs1' and 'cs2', and the 'cs3' is not taken into account.
    Message <simpleMessage>
      Skipping ANOVA due to the number of trials for the cs1 and/or cs2.
    Warning <simpleWarning>
      You have selected 3 CSs. At the moment the package does not support
                  the meta-analytic effect size for more than 2 stimuli. The reported meta-analytic effect
                  size for the ANOVA corresponds to only 'cs1' and 'cs2', and the 'cs3' is not taken into account.
      You have selected 3 CSs. At the moment the package does not support
                  the meta-analytic effect size for more than 2 stimuli. The reported meta-analytic effect
                  size for the ANOVA corresponds to only 'cs1' and 'cs2', and the 'cs3' is not taken into account.
      You have selected 3 CSs. At the moment the package does not support
                  the meta-analytic effect size for more than 2 stimuli. The reported meta-analytic effect
                  size for the ANOVA corresponds to only 'cs1' and 'cs2', and the 'cs3' is not taken into account.
      You have selected 3 CSs. At the moment the package does not support
                  the meta-analytic effect size for more than 2 stimuli. The reported meta-analytic effect
                  size for the ANOVA corresponds to only 'cs1' and 'cs2', and the 'cs3' is not taken into account.
      You have selected 3 CSs. At the moment the package does not support
                  the meta-analytic effect size for more than 2 stimuli. The reported meta-analytic effect
                  size for the ANOVA corresponds to only 'cs1' and 'cs2', and the 'cs3' is not taken into account.
      You have selected 3 CSs. At the moment the package does not support
                  the meta-analytic effect size for more than 2 stimuli. The reported meta-analytic effect
                  size for the ANOVA corresponds to only 'cs1' and 'cs2', and the 'cs3' is not taken into account.
      You have selected 3 CSs. At the moment the package does not support
                  the meta-analytic effect size for more than 2 stimuli. The reported meta-analytic effect
                  size for the ANOVA corresponds to only 'cs1' and 'cs2', and the 'cs3' is not taken into account.
      You have selected 3 CSs. At the moment the package does not support
                  the meta-analytic effect size for more than 2 stimuli. The reported meta-analytic effect
                  size for the ANOVA corresponds to only 'cs1' and 'cs2', and the 'cs3' is not taken into account.
      You have selected 3 CSs. At the moment the package does not support
                  the meta-analytic effect size for more than 2 stimuli. The reported meta-analytic effect
                  size for the ANOVA corresponds to only 'cs1' and 'cs2', and the 'cs3' is not taken into account.
      You have selected 3 CSs. At the moment the package does not support
                  the meta-analytic effect size for more than 2 stimuli. The reported meta-analytic effect
                  size for the ANOVA corresponds to only 'cs1' and 'cs2', and the 'cs3' is not taken into account.
      You have selected 3 CSs. At the moment the package does not support
                  the meta-analytic effect size for more than 2 stimuli. The reported meta-analytic effect
                  size for the ANOVA corresponds to only 'cs1' and 'cs2', and the 'cs3' is not taken into account.
      You have selected 3 CSs. At the moment the package does not support
                  the meta-analytic effect size for more than 2 stimuli. The reported meta-analytic effect
                  size for the ANOVA corresponds to only 'cs1' and 'cs2', and the 'cs3' is not taken into account.
    Output
      # A tibble: 34 x 18
         x     y     exclusion cut_off model controls method p.value effect.size
         <chr> <chr> <chr>     <chr>   <chr> <lgl>    <chr>    <dbl>       <dbl>
       1 cs    scr   full_data full d~ t-te~ NA       t-tes~ 1.24e-7      0.455 
       2 cs    scr   full_data full d~ t-te~ NA       t-tes~ 2.47e-7      0.455 
       3 cs:t~ scr   full_data full d~ rep ~ NA       rep A~ 3.70e-9      0.0354
       4 cs    scr   full_data full d~ rep ~ NA       rep A~ 2.47e-7      0.0847
       5 cs    scr   ten_per   full d~ t-te~ NA       t-tes~ 6.49e-3      0.235 
       6 cs    scr   ten_per   full d~ t-te~ NA       t-tes~ 1.30e-2      0.235 
       7 cs:t~ scr   ten_per   full d~ rep ~ NA       rep A~ 6.96e-6      0.0811
       8 cs    scr   ten_per   full d~ rep ~ NA       rep A~ 1.30e-2      0.0121
       9 cs    scr   min_first full d~ t-te~ NA       t-tes~ 1.48e-7      0.554 
      10 cs    scr   min_first full d~ t-te~ NA       t-tes~ 2.97e-7      0.554 
      # ... with 24 more rows, and 9 more variables: effect.size.ma <dbl>,
      #   effect.size.ma.lci <dbl>, effect.size.ma.hci <dbl>, estimate <dbl>,
      #   statistic <dbl>, conf.low <dbl>, conf.high <dbl>, framework <chr>,
      #   data_used <list>

# multiverse works with groups

    Code
      multiverse_cs(cs1, cs2, subj = subj, data = example_data, group = group,
        include_bayes = FALSE)
    Warning <simpleWarning>
      You have selected 3 CSs. At the moment the package does not support
                  the meta-analytic effect size for more than 2 stimuli. The reported meta-analytic effect
                  size for the ANOVA corresponds to only 'cs1' and 'cs2', and the 'cs3' is not taken into account.
      You have selected 3 CSs. At the moment the package does not support
                  the meta-analytic effect size for more than 2 stimuli. The reported meta-analytic effect
                  size for the ANOVA corresponds to only 'cs1' and 'cs2', and the 'cs3' is not taken into account.
      You have selected 3 CSs. At the moment the package does not support
                  the meta-analytic effect size for more than 2 stimuli. The reported meta-analytic effect
                  size for the ANOVA corresponds to only 'cs1' and 'cs2', and the 'cs3' is not taken into account.
      You have selected 3 CSs. At the moment the package does not support
                  the meta-analytic effect size for more than 2 stimuli. The reported meta-analytic effect
                  size for the ANOVA corresponds to only 'cs1' and 'cs2', and the 'cs3' is not taken into account.
    Message <simpleMessage>
      Skipping ANOVA due to the number of trials for the cs1 and/or cs2.
    Warning <simpleWarning>
      You have selected 3 CSs. At the moment the package does not support
                  the meta-analytic effect size for more than 2 stimuli. The reported meta-analytic effect
                  size for the ANOVA corresponds to only 'cs1' and 'cs2', and the 'cs3' is not taken into account.
      You have selected 3 CSs. At the moment the package does not support
                  the meta-analytic effect size for more than 2 stimuli. The reported meta-analytic effect
                  size for the ANOVA corresponds to only 'cs1' and 'cs2', and the 'cs3' is not taken into account.
      You have selected 3 CSs. At the moment the package does not support
                  the meta-analytic effect size for more than 2 stimuli. The reported meta-analytic effect
                  size for the ANOVA corresponds to only 'cs1' and 'cs2', and the 'cs3' is not taken into account.
      You have selected 3 CSs. At the moment the package does not support
                  the meta-analytic effect size for more than 2 stimuli. The reported meta-analytic effect
                  size for the ANOVA corresponds to only 'cs1' and 'cs2', and the 'cs3' is not taken into account.
      You have selected 3 CSs. At the moment the package does not support
                  the meta-analytic effect size for more than 2 stimuli. The reported meta-analytic effect
                  size for the ANOVA corresponds to only 'cs1' and 'cs2', and the 'cs3' is not taken into account.
      You have selected 3 CSs. At the moment the package does not support
                  the meta-analytic effect size for more than 2 stimuli. The reported meta-analytic effect
                  size for the ANOVA corresponds to only 'cs1' and 'cs2', and the 'cs3' is not taken into account.
      You have selected 3 CSs. At the moment the package does not support
                  the meta-analytic effect size for more than 2 stimuli. The reported meta-analytic effect
                  size for the ANOVA corresponds to only 'cs1' and 'cs2', and the 'cs3' is not taken into account.
      You have selected 3 CSs. At the moment the package does not support
                  the meta-analytic effect size for more than 2 stimuli. The reported meta-analytic effect
                  size for the ANOVA corresponds to only 'cs1' and 'cs2', and the 'cs3' is not taken into account.
      You have selected 3 CSs. At the moment the package does not support
                  the meta-analytic effect size for more than 2 stimuli. The reported meta-analytic effect
                  size for the ANOVA corresponds to only 'cs1' and 'cs2', and the 'cs3' is not taken into account.
      You have selected 3 CSs. At the moment the package does not support
                  the meta-analytic effect size for more than 2 stimuli. The reported meta-analytic effect
                  size for the ANOVA corresponds to only 'cs1' and 'cs2', and the 'cs3' is not taken into account.
      You have selected 3 CSs. At the moment the package does not support
                  the meta-analytic effect size for more than 2 stimuli. The reported meta-analytic effect
                  size for the ANOVA corresponds to only 'cs1' and 'cs2', and the 'cs3' is not taken into account.
      You have selected 3 CSs. At the moment the package does not support
                  the meta-analytic effect size for more than 2 stimuli. The reported meta-analytic effect
                  size for the ANOVA corresponds to only 'cs1' and 'cs2', and the 'cs3' is not taken into account.
    Output
      # A tibble: 34 x 18
         x     y     exclusion cut_off model controls method p.value effect.size
         <chr> <chr> <chr>     <chr>   <chr> <lgl>    <chr>    <dbl>       <dbl>
       1 cs    scr   full_data full d~ t-te~ NA       t-tes~ 3.77e-3     0.455  
       2 cs    scr   full_data full d~ t-te~ NA       t-tes~ 7.53e-3     0.455  
       3 grou~ scr   full_data full d~ rep ~ NA       rep A~ 2.05e-1     0.00227
       4 grou~ scr   full_data full d~ rep ~ NA       rep A~ 7.52e-1    -0.00230
       5 cs    scr   ten_per   full d~ t-te~ NA       t-tes~ 1.44e-1     0.235  
       6 cs    scr   ten_per   full d~ t-te~ NA       t-tes~ 2.87e-1     0.235  
       7 grou~ scr   ten_per   full d~ rep ~ NA       rep A~ 7.71e-1    -0.00322
       8 grou~ scr   ten_per   full d~ rep ~ NA       rep A~ 7.32e-1    -0.00192
       9 cs    scr   min_first full d~ t-te~ NA       t-tes~ 8.43e-4     0.554  
      10 cs    scr   min_first full d~ t-te~ NA       t-tes~ 1.69e-3     0.554  
      # ... with 24 more rows, and 9 more variables: effect.size.ma <dbl>,
      #   effect.size.ma.lci <dbl>, effect.size.ma.hci <dbl>, estimate <dbl>,
      #   statistic <dbl>, conf.low <dbl>, conf.high <dbl>, framework <chr>,
      #   data_used <list>

