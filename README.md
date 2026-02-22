# eco230r

**eco230r** is an R package containing helper functions, datasets, and workflows for the University of Wisconsin–La Crosse course **ECO 230: Data Analysis**.  
The package standardizes common descriptive statistics, tables, plots, and hypothesis testing tasks used throughout the course.

The goal is to reduce repetitive coding, promote consistent output, and allow students to focus on interpretation rather than syntax.

---

## 📦 What This Package Provides

- Functions for descriptive statistics and frequency tables  
- Tools for cross-tabulations and grouped summaries  
- Helper functions for hypothesis tests (t-tests, ANOVA, chi-square, etc.)  
- Built-in datasets used in ECO 230 assignments  
- Consistent formatting for tables and output  

---

## 🔧 Installation

```r
# install.packages("remotes")  # if not already installed
remotes::install_github("ECO230/eco230r")
```

```r
library(eco230r)
```

---

## 📚 Viewing Available Functions

```r
ls("package:eco230r")
```

```r
?function_name
```

---

## 📖 Function Reference (Quick Guide)

These are the **core student-facing functions** used regularly in ECO 230.

| Function | What it does | Typical inputs | Returns (key elements) |
|---|---|---|---|
| `ano()` | One-way ANOVA with assumption check. Uses Levene’s test for homogeneity of variance; runs classic `aov()` + Tukey HSD when HOV holds, or robust one-way ANOVA (`WRS2::t1way`) + linear-constraint post hocs when HOV fails. | `dv ~ group` (formula) and optional `data`; optional tuning: `tr` (trim), `min_n`, `max_cat` | List with: `analysis_type`, `results`, `descriptive_statistics`, `post_hoc_analysis` |
| `csf()` | Chi-square **goodness-of-fit** test for one categorical variable, with optional expected proportions / counts. | `~cat` (formula) and optional `data`, or a factor/vector; optional `probs` (vector or table) | List with: `analysis_type`, `results`, `fit_model`, `observed`, `expected`, `standardized_residuals`, `contribution`, `table_percentages` |
| `csi()` | Chi-square **test of association/independence** for two categorical variables (contingency table), including reporting helpers (and odds ratio output). | `row_cat ~ col_cat` (formula) and optional `data`, or two factors/vectors | List with: `analysis_type`, `results`, `odds_ratio`, `observed`, `expected`, `standardized_residuals`, `contribution`, `table_percentages`, `column_percentages`, `row_percentages` |
| `idt()` | Independent (two-sample) t-test for a numeric outcome by a two-group factor. | `dv ~ group` (formula) and optional `data`; `tails` = 1 or 2 | List with: `analysis_type`, `results`, `descriptive_statistics` |
| `idw()` | Wilcoxon rank-sum test (Mann–Whitney) for independent nonparametric samples. | `dv ~ group` (formula) and optional `data`; `tails` = 1 or 2 | List with: `analysis_type`, `results`, `descriptive_statistics` |
| `ost()` | One-sample t-test comparing a numeric variable to a hypothesized mean `mu`. | `~dv` or `dv ~ 1` (formula) and optional `data`, or a numeric vector; `mu`, `tails` | List with: `analysis_type`, `results`, `descriptive_statistics` |
| `pst()` | Paired-samples t-test comparing two measurements from the same subjects. | Two formulas (e.g., `~x`, `~y`) with optional `data`, or two numeric vectors; `tails` | List with: `analysis_type`, `results`, `descriptive_statistics` |
| `psw()` | Wilcoxon signed-rank test for paired nonparametric samples. | Two formulas (e.g., `~x`, `~y`) with optional `data`, or two numeric vectors; `tails` | List with: `analysis_type`, `results`, `descriptive_statistics` |
| `slr()` | Simple linear regression (`lm`) with reporting helpers. | `y ~ x` (formula) and optional `data` | List with: `analysis_type`, `results`, `linear_regression_model`, `predictors`, `coefficients` |

---

## 🧪 Example Usage

```r
library(eco230r)

df <- read_csv('my_data.csv') 

df %>%
idt(scones ~ tea)

```

---

## 📁 Repository Structure

```
eco230r/
├── R/            # R function source files
├── man/          # Documentation (.Rd files)
├── data/         # Package datasets
├── DESCRIPTION   # Package metadata
├── NAMESPACE     # Exported functions
├── packrat/      # Dependency management
└── eco230r.Rproj # RStudio project
```

---

## 🎯 Design Philosophy

- Prioritize clarity over cleverness  
- Minimize required arguments  
- Consistent naming and output formats  
- Informative error messages  
- Support reproducible analysis  

---

## 🧑‍🎓 For Students

You are expected to:

- Understand what each function does conceptually  
- Interpret results in words  
- Not treat the package as a black box  

---

## 📄 License

Educational use for ECO 230.

---

## ✉️ Contact

Maintained by mboland@uwlax.edu
