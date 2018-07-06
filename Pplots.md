---
title: "Pplots"
author: "Nick Hedger"
date: "06/07/2018"
output: 
  html_document: 
    keep_md: yes
---


```r
library(drake)
plot_env=readd(PLOT)

plist=ls(plot_env)[str_detect(ls(plot_env), 'XPLOT')]
plist2=ls(plot_env)[str_detect(ls(plot_env), 'XYPLOT')]

for (i in 1:length(plist)){
print(get(plist[i],plot_env))
print(get(plist2[i],plot_env))
}
```

![](Pplots_files/figure-html/content-1.png)<!-- -->

```
## Warning: Removed 2176 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-2.png)<!-- -->![](Pplots_files/figure-html/content-3.png)<!-- -->

```
## Warning: Removed 2864 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-4.png)<!-- -->![](Pplots_files/figure-html/content-5.png)<!-- -->

```
## Warning: Removed 855 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-6.png)<!-- -->![](Pplots_files/figure-html/content-7.png)<!-- -->

```
## Warning: Removed 1552 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-8.png)<!-- -->![](Pplots_files/figure-html/content-9.png)<!-- -->

```
## Warning: Removed 810 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-10.png)<!-- -->![](Pplots_files/figure-html/content-11.png)<!-- -->

```
## Warning: Removed 1890 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-12.png)<!-- -->![](Pplots_files/figure-html/content-13.png)<!-- -->

```
## Warning: Removed 1108 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-14.png)<!-- -->![](Pplots_files/figure-html/content-15.png)<!-- -->

```
## Warning: Removed 1688 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-16.png)<!-- -->![](Pplots_files/figure-html/content-17.png)<!-- -->

```
## Warning: Removed 2465 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-18.png)<!-- -->![](Pplots_files/figure-html/content-19.png)<!-- -->

```
## Warning: Removed 6875 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-20.png)<!-- -->![](Pplots_files/figure-html/content-21.png)<!-- -->

```
## Warning: Removed 665 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-22.png)<!-- -->![](Pplots_files/figure-html/content-23.png)<!-- -->

```
## Warning: Removed 3550 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-24.png)<!-- -->![](Pplots_files/figure-html/content-25.png)<!-- -->

```
## Warning: Removed 820 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-26.png)<!-- -->![](Pplots_files/figure-html/content-27.png)<!-- -->

```
## Warning: Removed 311 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-28.png)<!-- -->![](Pplots_files/figure-html/content-29.png)<!-- -->

```
## Warning: Removed 2740 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-30.png)<!-- -->![](Pplots_files/figure-html/content-31.png)<!-- -->

```
## Warning: Removed 1539 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-32.png)<!-- -->![](Pplots_files/figure-html/content-33.png)<!-- -->

```
## Warning: Removed 2301 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-34.png)<!-- -->![](Pplots_files/figure-html/content-35.png)<!-- -->

```
## Warning: Removed 1751 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-36.png)<!-- -->![](Pplots_files/figure-html/content-37.png)<!-- -->

```
## Warning: Removed 577 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-38.png)<!-- -->![](Pplots_files/figure-html/content-39.png)<!-- -->

```
## Warning: Removed 1346 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-40.png)<!-- -->![](Pplots_files/figure-html/content-41.png)<!-- -->

```
## Warning: Removed 793 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-42.png)<!-- -->![](Pplots_files/figure-html/content-43.png)<!-- -->

```
## Warning: Removed 1718 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-44.png)<!-- -->![](Pplots_files/figure-html/content-45.png)<!-- -->

```
## Warning: Removed 2232 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-46.png)<!-- -->![](Pplots_files/figure-html/content-47.png)<!-- -->

```
## Warning: Removed 657 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-48.png)<!-- -->![](Pplots_files/figure-html/content-49.png)<!-- -->

```
## Warning: Removed 997 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-50.png)<!-- -->![](Pplots_files/figure-html/content-51.png)<!-- -->

```
## Warning: Removed 2461 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-52.png)<!-- -->![](Pplots_files/figure-html/content-53.png)<!-- -->

```
## Warning: Removed 554 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-54.png)<!-- -->![](Pplots_files/figure-html/content-55.png)<!-- -->

```
## Warning: Removed 4635 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-56.png)<!-- -->![](Pplots_files/figure-html/content-57.png)<!-- -->

```
## Warning: Removed 101 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-58.png)<!-- -->![](Pplots_files/figure-html/content-59.png)<!-- -->

```
## Warning: Removed 727 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-60.png)<!-- -->![](Pplots_files/figure-html/content-61.png)<!-- -->

```
## Warning: Removed 2842 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-62.png)<!-- -->![](Pplots_files/figure-html/content-63.png)<!-- -->

```
## Warning: Removed 611 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-64.png)<!-- -->![](Pplots_files/figure-html/content-65.png)<!-- -->

```
## Warning: Removed 605 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-66.png)<!-- -->![](Pplots_files/figure-html/content-67.png)<!-- -->

```
## Warning: Removed 787 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-68.png)<!-- -->![](Pplots_files/figure-html/content-69.png)<!-- -->

```
## Warning: Removed 2249 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-70.png)<!-- -->![](Pplots_files/figure-html/content-71.png)<!-- -->

```
## Warning: Removed 2286 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-72.png)<!-- -->![](Pplots_files/figure-html/content-73.png)<!-- -->

```
## Warning: Removed 1358 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-74.png)<!-- -->![](Pplots_files/figure-html/content-75.png)<!-- -->

```
## Warning: Removed 3693 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-76.png)<!-- -->![](Pplots_files/figure-html/content-77.png)<!-- -->

```
## Warning: Removed 3539 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-78.png)<!-- -->![](Pplots_files/figure-html/content-79.png)<!-- -->

```
## Warning: Removed 938 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-80.png)<!-- -->![](Pplots_files/figure-html/content-81.png)<!-- -->

```
## Warning: Removed 996 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-82.png)<!-- -->![](Pplots_files/figure-html/content-83.png)<!-- -->

```
## Warning: Removed 2040 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-84.png)<!-- -->![](Pplots_files/figure-html/content-85.png)<!-- -->

```
## Warning: Removed 658 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-86.png)<!-- -->![](Pplots_files/figure-html/content-87.png)<!-- -->

```
## Warning: Removed 1733 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-88.png)<!-- -->![](Pplots_files/figure-html/content-89.png)<!-- -->

```
## Warning: Removed 1654 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-90.png)<!-- -->![](Pplots_files/figure-html/content-91.png)<!-- -->

```
## Warning: Removed 2982 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-92.png)<!-- -->![](Pplots_files/figure-html/content-93.png)<!-- -->

```
## Warning: Removed 3326 rows containing missing values (geom_point).
```

![](Pplots_files/figure-html/content-94.png)<!-- -->
