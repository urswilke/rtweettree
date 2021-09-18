# generated tbl graph object still the same

    Code
      g %>% as_tibble()
    Output
      # A tibble: 9 x 7
        name                type  screen_name url      text     label   dist_to_center
        <chr>               <chr> <chr>       <glue>   <chr>    <chr>            <dbl>
      1 1438476950746636291 user  rtweetbird1 https:/~ <NA>     rtweet~              1
      2 1438480252003569671 user  rtweetbird3 https:/~ <NA>     rtweet~              2
      3 1438479415550390275 user  rtweetbird2 https:/~ <NA>     rtweet~              1
      4 1438481824922181635 tweet rtweetbird1 https:/~ this is~ this i~              0
      5 1438483457697591297 tweet rtweetbird3 https:/~ @rtweet~ @rtwee~              1
      6 1438482432030818307 tweet rtweetbird2 https:/~ @rtweet~ @rtwee~              1
      7 1438482309490040835 tweet rtweetbird2 https:/~ @rtweet~ @rtwee~              1
      8 1438484289616859145 tweet rtweetbird3 https:/~ this is~ this i~              1
      9 1438483563314360322 tweet rtweetbird3 https:/~ @rtweet~ @rtwee~              2

---

    Code
      g %E>% as_tibble()
    Output
      # A tibble: 19 x 5
          from    to user_id             screen_name type   
         <int> <int> <chr>               <chr>       <chr>  
       1     4     5 1438480252003569671 rtweetbird3 reply  
       2     4     6 1438479415550390275 rtweetbird2 reply  
       3     4     7 1438479415550390275 rtweetbird2 reply  
       4     4     8 1438480252003569671 rtweetbird3 quote  
       5     6     9 1438480252003569671 rtweetbird3 reply  
       6     8     1 1438476950746636291 rtweetbird3 like   
       7     9     1 1438476950746636291 rtweetbird3 like   
       8     5     1 1438476950746636291 rtweetbird3 like   
       9     6     1 1438476950746636291 rtweetbird2 like   
      10     7     1 1438476950746636291 rtweetbird2 like   
      11     6     2 1438480252003569671 rtweetbird2 like   
      12     4     1 1438476950746636291 rtweetbird1 by     
      13     5     2 1438480252003569671 rtweetbird3 by     
      14     6     3 1438479415550390275 rtweetbird2 by     
      15     7     3 1438479415550390275 rtweetbird2 by     
      16     8     2 1438480252003569671 rtweetbird3 by     
      17     9     2 1438480252003569671 rtweetbird3 by     
      18     4     1 1438476950746636291 rtweetbird1 by     
      19     4     3 1438479415550390275 rtweetbird2 retweet

