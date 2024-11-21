Cross Validation
================
Chhiring Lama
2024-11-19

Look at LIDAR data

``` r
data("lidar")

lidar_df <- lidar |> 
  as_tibble() |> 
  mutate(id = row_number())
```

``` r
lidar_df |> 
  ggplot(aes(x = range, y = logratio)) + 
  geom_point()
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-2-1.png" width="85%" style="display: block; margin: auto;" />

## Try to do CV

Compare 3 models = one Linear, one smooth and one wiggly

Construct training and testing datasets

``` r
train_df <- sample_frac(
  lidar_df, 
  size = .8
)

test_df <- anti_join(lidar_df, train_df, by = "id")
```

Look at these

``` r
train_df |> 
  ggplot(aes(x = range, y = logratio)) + 
  geom_point()+
  geom_point(data = test_df, color = "red")
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-4-1.png" width="85%" style="display: block; margin: auto;" />

Fit the 3 models

``` r
linear_model = lm(logratio ~ range, data = train_df)
smooth_model = gam(logratio ~ s(range), data = train_df)
wiggly_model = gam(logratio ~ s(range, k = 30), data = train_df, sp = 10e-6)
```

Look at the fits

``` r
train_df |> 
  add_predictions(linear_model) |> 
  ggplot(aes(x = range, y = logratio)) + 
  geom_point()+
  geom_line(aes(y = pred), color = "red") +
  geom_point(data = test_df, color = "red")+
  labs(title = "Linear Model fit")
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-6-1.png" width="85%" style="display: block; margin: auto;" />

``` r
train_df |> 
  add_predictions(wiggly_model) |> 
  ggplot(aes(x = range, y = logratio)) + 
  geom_point()+
  geom_line(aes(y = pred), color = "red") +
  geom_point(data = test_df, color = "red")+
  labs(title = "Linear Model fit")
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-7-1.png" width="85%" style="display: block; margin: auto;" />

``` r
train_df |> 
  add_predictions(smooth_model) |> 
  ggplot(aes(x = range, y = logratio)) + 
  geom_point()+
  geom_line(aes(y = pred), color = "red") +
  geom_point(data = test_df, color = "red")+
  labs(title = "Linear Model fit")
```

<img src="cross_validation_files/figure-gfm/unnamed-chunk-8-1.png" width="85%" style="display: block; margin: auto;" />

Compare them numerically using RMSE

``` r
rmse(linear_model, test_df)
```

    ## [1] 0.127317

``` r
rmse(smooth_model, test_df)
```

    ## [1] 0.08302008

``` r
rmse(wiggly_model, test_df)
```

    ## [1] 0.08848557

Small difference between smooth and wiggly model. Will this be
consistent if we use different training and testing data
