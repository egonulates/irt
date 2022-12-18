

# irt 0.2.8
* Added `kappa_coef()` function to calculate Cohen's weighted/unweighted Kappa 
  coefficient.
* `Response` objects prints 'misc' fields without an error when there is a 
  factor type variable. 
* Fixed an error in the function `irt:::marginal_reliability(..., method = "empirical")`.



# irt 0.2.6

* Plot functions can now suppress plot titles by setting `title = NULL`. 
* `item_analysis()` function now has a new argument `stats` where only selected
  item analysis statistics can be calculated. 
* `item_analysis()` function can now calculate `pval_unadj` which is simply the 
  mean of item scores. `pval` is still the mean of item scores divided by each
  item's maximum possible score. 
* Added `area_between_icc()` function to calculate the area between two item 
  characteristic curves of two unidimensional dichotomous items. The function 
  can calculate either exact area (except when 3PL with different 
  pseudo-guessing parameters) or area between two theta values. 
* `cat_sim()` function can use MAP (maximum-a-posteriori or Bayes Modal) and 
  MAP + ML (i.e. MAP until an imperfect response string and ML after) ability
  estimation methods. 
* Responses, items administered, administered item IDs, estimates before or 
  after the administration of an item can be extracted from the individual 
  CAT output. For details, see "?`$.cat_output`". 
* Added `output_type` argument to `est_ability(, output_type = )` function with
  available following output types: `"list"`, `"data.frame"`, `"tibble"`. 
  Previously, the output was only a "list".

# irt 0.1.4

* Bug fix: An `Itempool` object with one testlet can be printed without an 
  error.
* Added `$item_ids` to testlet object to get the ids of the items within the
  testlet. 
* Fixed bugs in `plot_empirical_icc()` function. 


# irt 0.1.3

* Added `$max_score` for `Testlet` objects. For a given `Testlet` object `t1`, 
  `t1$max_score` returns the maximum obtainable score by all items within 
  the testlet. 
* Added `$resp_item_list` for `Itempool` objects. For an `Itempool` object 
  `ip`, `ip$resp_item_list` will combine items that are not in a testlet and 
  items within a testlet and returns a list object that is composed of only
  `Item` objects. 


# irt 0.1.2

* Package can be used in MacOS as well. Resolved compiler related issues. 
* Added `est_bilog()` function to estimate item parameters for "1PL", "2PL" and
"3PL" models using BILOG-MG on computers with Windows OS. This function 
requires an installed BILOG-MG program on the machine. 


# irt 0.1.0

* This is the initial release of the package. 

