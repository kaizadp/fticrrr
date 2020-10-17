fticrrr report
================

-----

## FTICR domains

### compound classes

``` r
readd(gg_vankrevelens)$gg_vk_domains+theme(legend.position = "right")
```

![](markdown-figs/fticrrr/domains_classes-1.png)<!-- -->

### NOSC

``` r
readd(gg_vankrevelens)$gg_vk_domains_nosc
```

![](markdown-figs/fticrrr/domains_nosc-1.png)<!-- -->

-----

## Van Krevelen plots

option 1

``` r
readd(gg_vankrevelens)$gg_vk1
```

![](markdown-figs/fticrrr/gg_vk1-1.png)<!-- -->

option 2, with marginal plots

``` r
readd(gg_vankrevelens)$gg_vk2
```

![](markdown-figs/fticrrr/gg_vk2-1.png)<!-- -->

-----

## PCA

``` r
readd(gg_pca)+theme_kp()+xlim(-3.5,3.5)+ylim(-3,3)
```

![](markdown-figs/fticrrr/PCA-1.png)<!-- -->

-----

## Relative abundance

``` r
readd(gg_relabund_bar)+theme(legend.position = "right")
```

![](markdown-figs/fticrrr/relabund-1.png)<!-- -->
