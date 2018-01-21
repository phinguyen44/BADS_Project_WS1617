# Notes on Feature Engineering

## Customer information

| FEATURE        | TYPE    | # FEATURES | HOW?                                       |
|----------------|---------|------------|--------------------------------------------|
| Gender/Group   | Factor  | 5          | user_title                                 |
| Age            | Numeric | 1          | transform user_dob (consider discretizing) |
| Age Group      | Factor  | 11         | Binning of age with equal interval         |
| Location       | Factor  | 16         | user_state                                 |
| West/East Ger  | Factor  | 2          | Transform user_state                       |
| Income age     | Numeric | 11         | External information & age                 |
| Income state   | Numeric | 16         | External information & user_state          |
| Income Indicat | Numeric | 1          | External information & user_state          |
| Purchases Made | Numeric | 1          | transform user_id and order_item_id        |
| Purchase Value | Numeric | 1          | transform user_id and order_item_id        |
| Return WOE     | Numeric | 1          | transform user_id using WOE                |
| Account age    | Numeric | 1          | transform  user_dob and order_date         |



## Order / basket level information

| FEATURE                    | TYPE    | # FEATURES | HOW?                                             |
|----------------------------|---------|------------|--------------------------------------------------|
| Order Size                 | Numeric | 1          | Transform from items on same day by same user_id |
| Delivery Time              | Numeric | 1          | Transform delivery_date                          |
| Order Year                 | Factor  | 2          | Transform order_date                             |
| Order Month                | Factor  | 12         | Transform order_date                             |
| Order Day of Week          | Factor  | 7          | Transform order_date                             |
| Basket Value               | Numeric | 1          | Transform order_date, user_id, item_price     |
| Basket Size                | Numeric | 1          | Transform order_date, user_id 
               |
| Basket size > 1            | Factor  | 2          | Transform order_date, user_id
               |
| Same item in basket (D)    | Factor  | 2          | Transform order_date, user_id, item_id        |
| Same items in basket       | Numeric | 1          | Transform order_date, user_id, item_id        |
| Same items in basket diff size  | Numeric | 1          | Transform order_date, user_id, item_id, item_size |
| Same items in basket diff size (D) | Factor | 2          | Transform order_date, user_id, item_id, item_size|
| No. items in basket same category  | Numeric | 1          | Transform order_date, user_id, item_id, item_category |
| No. items in basket same category (D) | Factor | 2          | Transform order_date, user_id, item_id, item_category |
| First order of customer | Factor | 2          | Transform order_date, user_reg_daze |
| Item discount (abs) | Numeric | 1          | Transform item_id, item_price, item_size                     |
| Item discount (pc) | Numeric | 1          | Transform item_id, item_price, item_size |




## Product level information

| FEATURE                                                  | TYPE    | # FEATURES | HOW?                                             |
|----------------------------------------------------------|---------|------------|--------------------------------------------------|
| Item Price                                               | Numeric | 1          | item_price (continous)                       |
| Item Price                                               | Factor  | 10         | item_price (discrete)                       |
| Zero Item Price                                          | Factor | 2          | item_price (discrete)                       |
| Item ID                                                  | Factor  | 2656       | WOE or malhanoblis                               |
| Brand ID                                                 | Factor  | 155        | WOE or malhanoblis                               |
| Item Color                                               | Factor  | 84         | Group same colors together, WOE or malhanoblis   |
or malhanoblis                               |
| Item basic Color                                               | Factor  | 9         | Group same basic colors together  |
| Item Size                                                | Factor  | 114        | Group same levels together, , WOE or malhanoblis |
| Product category (Shoes, Pants, clothing.)                   | Factor  | 5        | Group with unique sizes                                            |
| Product subcategory (cheap, neutral, luxus)                   | Factor  | 3        | Price quantile within category
| Product Category (Outdoor, Climbing, Urban, etc)         | Factor  | ???        | Can't                                            |

## Checkout process information

| FEATURE             | TYPE   | # FEATURES | HOW?  |
|---------------------|--------|------------|-------|
| Which checkout flow | Factor | ???        | can't |
| Which sales channel | Factor | ???        | can't |
