library(rvest)
library(dplyr)
library(stringr)
library(ggplot2)

# Function to parse FastQC HTML module status table
parse_fastqc_html <- function(file, label) {
  html <- read_html(file)
  
  # Try finding the first table (summary table)
  table <- html %>%
    html_nodes("table") %>%
    .[[1]] %>% 
    html_table(fill = TRUE)
  
  # Clean + tag
  colnames(table) <- c("Status", "Module")
  sample_id <- basename(file) %>% str_remove("_fastqc\\.html|_ORDERED.*")
  
  table %>%
    mutate(Sample = sample_id,
           Type = label)
}

# BEFORE: Only in qc_logs (not repaired), not containing ORDERED
before_htmls <- list.files(
  path = getwd(),
  pattern = "_[12]_fastqc\\.html$",
  recursive = TRUE,
  full.names = TRUE
) %>%
  grep("qc_logs/", ., value = TRUE) %>%
  grep("ORDERED", ., invert = TRUE, value = TRUE)

# AFTER: Only in qc_logs_repaired and containing ORDERED
after_htmls <- list.files(
  path = getwd(),
  pattern = "ORDERED_[12]_fastqc\\.html$",
  recursive = TRUE,
  full.names = TRUE
) %>%
  grep("qc_logs_repaired/", ., value = TRUE)

qc_before <- lapply(before_htmls, parse_fastqc_html, label = "Before") %>% bind_rows()
qc_after  <- lapply(after_htmls, parse_fastqc_html, label = "After") %>% bind_rows()

qc_combined <- bind_rows(qc_before, qc_after)

ggplot(qc_combined, aes(x = Module, fill = Status)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ Type) +
  theme_minimal(base_size = 12) +
  labs(
    title = "FastQC Summary: Before vs After Preprocessing",
    x = "QC Module",
    y = "Number of Samples"
  ) +
  scale_fill_manual(values = c("PASS" = "seagreen", "FAIL" = "firebrick", "WARN" = "goldenrod")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

