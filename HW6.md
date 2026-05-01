    library(vitals)
    library(ellmer)
    library(dplyr)

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    library(tibble)

    eval_data <- tibble(
      id = c("basic_filter", "financial_filter", "subjective_edge_case"),
      input = c(
        "Show me data for Dallas and Austin after 2010",
        "Filter to months where the median price was over $200,000",
        "Show me the cities with the worst inventory turnover rates."
      ),
      target = c(
        "The solution must filter the `city` column for Dallas and Austin, and the `year` column for > 2010.",
        "The solution must filter the `median` column for > 200000.",
        "Award 'C' if the code explicitly uses `arrange(desc(inventory))` or `arrange(inventory)`. Award 'P' if the code filters the inventory column but does not sort it. Award 'I' if the code ignores the inventory column entirely."  )
    )

    tx_task <- Task$new(
      dataset = eval_data,
      solver = generate(chat_anthropic(
        model = "claude-haiku-4-5",
        system_prompt = "You are a Texas Real Estate Data Analyst. Keep responses brief and focused on writing dplyr code to filter the txhousing dataset."
      )),
      scorer = model_graded_qa(partial_credit = TRUE),
      name = "Texas Housing Filtering Eval"
    )

    ## Warning: ! vitals could not find a log directory; evaluation log files will be written
    ##   to a temporary directory.
    ## ℹ Set a log directory with e.g. `vitals::vitals_log_dir_set("./logs")`, perhaps
    ##   in '~/.Rprofile', to quiet this warning.

    tx_task$eval()

    ## ℹ Solving

    ## ✔ Solving [2.7s]
    ## ℹ Scoring[working] (0 + 0) -> 2 -> 1 | ■■■■■■■■■■■                       33%[working] (0 + 0) -> 1 -> 2 | ■■■■■■■■■■■■■■■■■■■■■             67%                                                                    ℹ Scoring✔ Scoring [2.9s]

    tx_results <- vitals_bind(tx_task)
    print(tx_results)

    ## # A tibble: 3 × 4
    ##   task    id                   score metadata        
    ##   <chr>   <chr>                <ord> <list>          
    ## 1 tx_task basic_filter         C     <tibble [1 × 7]>
    ## 2 tx_task financial_filter     C     <tibble [1 × 7]>
    ## 3 tx_task subjective_edge_case P     <tibble [1 × 7]>

# STAT 6395 Homework 6: App Evaluation

## 1. App Overview

-   **Dataset Used:** `ggplot2::txhousing` (8,602 rows).
-   **Model Used:** `claude-haiku-4-5`

## 2. QueryChat Filtering Evaluation

I tested the filtering capabilities using the data descriptions provided
to the model.

-   **Test A (Basic Filtering):** “Show me data for Dallas and Austin
    after 2010”
    -   **Result:** Worked successfully. The model correctly filtered
        `city %in% c("Dallas", "Austin")` and `year > 2010`.
-   **Test B (Financial):** “Filter to months where the median price was
    over $200,000”
    -   **Result:** Worked kinda successfully. It did show prices over
        $200,000 but it kept only showing Dallas and Austin from the
        previous prompt.
-   **Test C (Edge):** “Show me the cities with the worst inventory
    turnover rates.”
    -   **Result:** The model struggled a little. It asked a followup
        question and then got the answer correct and displayed properly.

## 3. Vision Model (Plot Interpretation) Evaluation

I used a token-efficient system prompt (“Interpret plot in &lt;20 words.
State 1 key trend. Suggest 1 follow-up.”) to evaluate the vision
capabilities.

-   **Test A (Trend):** Scatterplot of `year` vs `median` price.

    -   **Result:** The model successfully identified the positive
        correlation over time in under 20 words, following the
        token-efficient constraints.

-   **Test B (Overplotting Struggle):** Scatterplot of sales vs volume
    with no filtering (8,000+ points).

    -   **Result:** The model struggled. Due to massive overplotting at
        the origin, the vision model had difficulty distinguishing
        individual outliers from the dense cluster, hallucinating
        specific values based on the axes rather than the points
        themselves.

## 4. Vitals Package Exploration (Bonus)

To test the chatbot, I used the vitals package to run an automated
evaluation. I set up a dataset of test questions and grading rules, then
tested the claude-haiku-4-5 model using an AI judge that allowed for
partial credit. The results were strong: the model scored a ‘C’
(Correct) on both standard data filtering tests. However, when handling
a tricky subjective question like finding the ‘worst’ inventory
turnover, the judge awarded a ‘P’ (Partially Correct) likely due to the
ambiguity of the request.
