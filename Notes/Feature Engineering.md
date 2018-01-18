# Notes on Feature Engineering

## Demographic information

| FEATURE        | TYPE    | # FEATURES | HOW?                                       |
|----------------|---------|------------|--------------------------------------------|
| Gender         | Factor  | 5          | user_title                                 |
| Age            | Numeric | 1          | transform user_dob (consider discretizing) |
| Location       | Factor  | 16         | user_state                                 |
| Purchases Made | Numeric | 1          | transform using rank() or row_number       |
| Return WOE     | Numeric | 1          | transform user_id using WOE                |

## Order / basket level information

| FEATURE                    | TYPE    | # FEATURES | HOW?                                             |
|----------------------------|---------|------------|--------------------------------------------------|
| Order Size                 | Numeric | 1          | Transform from items on same day by same user_id |
| Delivery Time              | Numeric | 1          | Transform delivery_date                          |
| Order Year                 | Factor  | 2          | Transform order_date                             |
| Order Month                | Factor  | 12         | Transform order_date                             |
| Order Day of Week          | Factor  | 7          | Transform order_date                             |
| Order Hour                 | Factor  | 24         | Can't                                            |
| Payment Method             | Factor  | ???        | Can't                                            |
| Platform (mobile, desktop) | Factor  | ???        | Can't                                            |

## Product level information

| FEATURE                                                  | TYPE    | # FEATURES | HOW?                                             |
|----------------------------------------------------------|---------|------------|--------------------------------------------------|
| Item Price                                               | Numeric | 1          | item_price (consider discretization)             |
| Item ID                                                  | Factor  | 2656       | WOE or malhanoblis                               |
| Brand ID                                                 | Factor  | 155        | WOE or malhanoblis                               |
| Item Color                                               | Factor  | 84         | Group same colors together, WOE or malhanoblis   |
| Item Size                                                | Factor  | 114        | Group same levels together, , WOE or malhanoblis |
| # Same Item in Basket                                    | Numeric | 1          | Transform by order on same date                  |
| # Items with Same Color, Brand, Different Size in Basket | Numeric | 1          | Transform                                        |
| # Items with Same Color, Size, Different Brand in Basket | Numeric | 1          | Transform                                        |
| # Items with Same Brand, Size, Different Color in Basket | Numeric | 1          | Transform                                        |
| Product Category (Outdoor, Climbing, Urban, etc)         | Factor  | ???        | Can't                                            |
| Product Type (Shoes, Pants, etc.)                        | Factor  | ???        | Can't                                            |

## Checkout process information

| FEATURE             | TYPE   | # FEATURES | HOW?  |
|---------------------|--------|------------|-------|
| Which checkout flow | Factor | ???        | can't |
| Which sales channel | Factor | ???        | can't |
