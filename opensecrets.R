# Pull itemized individual political contributions from people who list
# Anthropic as their employer, via the OpenFEC API (schedule A).
#
# Setup:
#   install.packages(c("httr2", "dplyr", "purrr", "readr", "tibble"))
#   Get a free key at https://api.data.gov/signup/ (1,000 req/hr).
#   Put FEC_API_KEY=your_key in ~/.Renviron. DEMO_KEY works but is ~30 req/hr.

library(httr2)
library(dplyr)
library(purrr)
library(readr)
library(tibble)

api_key <- Sys.getenv("FEC_API_KEY", unset = "DEMO_KEY")

# Employer strings are free text, so query several spellings.
employer_variants <- c(
    "Anthropic",
    "Anthropic PBC",
    "Anthropic, PBC",
    "Anthropic Inc"
)

# Anthropic was founded in 2021; FEC two-year periods are keyed by the even year.
cycles <- c(2022, 2024, 2026)

fetch_cycle <- function(employer, cycle, per_page = 100) {
    out <- list()
    last_index <- NULL
    last_date <- NULL

    repeat {
        req <- request("https://api.open.fec.gov/v1/schedules/schedule_a/") |>
            req_url_query(
                api_key = api_key,
                contributor_employer = employer,
                two_year_transaction_period = cycle,
                is_individual = "true",
                per_page = per_page,
                sort = "-contribution_receipt_date",
                last_index = last_index,
                last_contribution_receipt_date = last_date
            ) |>
            req_retry(max_tries = 5, is_transient = \(r) {
                resp_status(r) %in% c(429, 500, 502, 503)
            }) |>
            req_throttle(rate = 20 / 60) # stay well under limits

        resp <- req_perform(req) |> resp_body_json()
        results <- resp$results
        if (length(results) == 0) {
            break
        }

        out <- c(out, results)

        idx <- resp$pagination$last_indexes
        if (is.null(idx) || is.null(idx$last_index)) {
            break
        }
        last_index <- idx$last_index
        last_date <- idx$last_contribution_receipt_date
    }

    if (length(out) == 0) {
        return(tibble())
    }

    map_dfr(out, \(x) {
        tibble(
            sub_id = x$sub_id,
            cycle = x$two_year_transaction_period,
            date = x$contribution_receipt_date,
            amount = x$contribution_receipt_amount,
            contributor_name = x$contributor_name,
            contributor_employer = x$contributor_employer,
            contributor_occupation = x$contributor_occupation,
            contributor_city = x$contributor_city,
            contributor_state = x$contributor_state,
            committee_id = x$committee$committee_id,
            committee_name = x$committee$name,
            committee_type = x$committee$committee_type_full,
            party = x$committee$party_full,
            memo_code = x$memo_code,
            receipt_type = x$receipt_type_full,
            filing_url = x$pdf_url
        )
    })
}

grid <- expand.grid(
    employer = employer_variants,
    cycle = cycles,
    stringsAsFactors = FALSE
)

contribs <- pmap_dfr(grid, \(employer, cycle) {
    message("Fetching: ", employer, " / ", cycle)
    fetch_cycle(employer, cycle)
}) |>
    distinct(sub_id, .keep_all = TRUE)

# The API's employer filter is a text match, so audit what actually came back
# (e.g. "Anthropic Consulting" or similar false positives) before analysis.
print(count(contribs, contributor_employer, sort = TRUE))

# Memo-coded rows (memo_code == "X") are duplicates of an earmark/conduit
# transaction; exclude them from totals.
clean <- contribs |> filter(is.na(memo_code) | memo_code != "X")

# Quick summaries
by_cycle <- clean |>
    group_by(cycle) |>
    summarise(
        n = n(),
        donors = n_distinct(contributor_name),
        total = sum(amount),
        .groups = "drop"
    )

by_recipient <- clean |>
    group_by(committee_name, cycle) |>
    summarise(n = n(), total = sum(amount), .groups = "drop") |>
    arrange(desc(total))

by_contributor <- contribs |>
    group_by(contributor_name, cycle) |>
    summarise(n = n(), total = sum(amount), .groups = "drop") |>
    arrange(desc(total))

print(by_cycle)
print(head(by_recipient, 25))

write_csv(contribs, "anthropic_contributions_raw.csv")
write_csv(by_recipient, "anthropic_contributions_by_recipient.csv")
