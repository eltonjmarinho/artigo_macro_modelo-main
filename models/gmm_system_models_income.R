# --- Estimação de Modelos GMM Sistêmicos por Grupo de Renda ---

# Suppress warnings immediately (including package loading); restore on exit
old_warn <- getOption("warn")
options(warn = -1)
on.exit(options(warn = old_warn), add = TRUE)

suppressWarnings({
  cat("Carregando pacotes necessários (plm, dplyr, AER)...\n")
  library(plm)
  library(dplyr)
  library(AER)

  cat("Carregando e preparando a base de dados...\n")
  data_full <- tryCatch({
    read.csv("data/processed/data_model.csv") %>%
      mutate(
        log_gdp = log(gdp),
        log_lab_part = log(lab_part),
        log_edu = log(edu),
        log_inv = log(inv)
      ) %>%
      arrange(iso3c, year) %>%
      group_by(iso3c) %>%
      mutate(
        log_gdp_lag1 = lag(log_gdp, 1),
        log_gdp_lag2 = lag(log_gdp, 2),
        log_gdp_lag3 = lag(log_gdp, 3)
      ) %>%
      ungroup() %>%
      filter(
        is.finite(log_gdp) &
        is.finite(log_inv) &
        is.finite(log_lab_part) &
        is.finite(log_edu) &
        is.finite(ef) &
        !is.na(wb_income_group)
      )
  }, error = function(e) {
    stop("Erro ao carregar base: ", e$message)
  })

  cat("Base de dados carregada.\n")

  reports_dir <- "reports"
  if (!dir.exists(reports_dir)) {
    dir.create(reports_dir, recursive = TRUE)
    cat("Pasta 'reports' criada.\n")
  }

  income_results <- list()
  income_tests <- list()

  # Fórmula simplificada para evitar explosão de instrumentos
  model_formula <- log_gdp ~ lag(log_gdp, 1) + log_inv + log_lab_part +
    log_edu + ef | lag(log_gdp, 2:3)

  cat("Usando fórmula GMM: ", deparse(model_formula), "\n")

  wu_formula <- log_gdp ~ log_gdp_lag1 + log_inv + log_lab_part + log_edu + ef |
    log_gdp_lag2 + log_gdp_lag3

  run_wu_hausman <- function(df, formula) {
    iv_fit <- try(AER::ivreg(formula = formula, data = df), silent = TRUE)

    if (inherits(iv_fit, "try-error")) {
      return(NULL)
    }

    diagnostics <- summary(iv_fit, diagnostics = TRUE)$diagnostics

    if (is.null(diagnostics)) {
      return(NULL)
    }

    label <- if ("Wu-Hausman F" %in% rownames(diagnostics)) {
      "Wu-Hausman F"
    } else if ("Wu-Hausman" %in% rownames(diagnostics)) {
      "Wu-Hausman"
    } else {
      NULL
    }

    if (is.null(label)) {
      return(NULL)
    }

    stats <- diagnostics[label, , drop = FALSE]

    pull_value <- function(col) {
      if (col %in% colnames(stats)) stats[1, col] else NA_real_
    }

    list(
      statistic = pull_value("statistic"),
      p_value = pull_value("p-value"),
      df1 = pull_value("df1"),
      df2 = pull_value("df2")
    )
  }

  report_hansen_j <- function(model) {
    hansen <- tryCatch(sargan(model), error = function(e) NULL)

    if (is.null(hansen)) {
      cat("Hansen J-test: não foi possível calcular para este grupo.\n")
      return(list(statistic = NA_real_, p_value = NA_real_, df = NA_real_))
    }

    stat <- ifelse(is.null(hansen$statistic), NA_real_, as.numeric(hansen$statistic))
    pval <- ifelse(is.null(hansen$p.value), NA_real_, as.numeric(hansen$p.value))
    df_num <- ifelse(is.null(hansen$parameter), NA_real_, as.numeric(hansen$parameter))

    if (is.na(stat) || is.na(pval)) {
      cat("Hansen J-test: não foi possível calcular para este grupo.\n")
      return(list(statistic = NA_real_, p_value = NA_real_, df = df_num))
    }

    df_txt <- ifelse(is.na(df_num), "-", formatC(df_num, format = "f", digits = 0))
    cat(sprintf("Hansen J-test: J = %.3f (df=%s), p-valor = %.4f\n", stat, df_txt, pval))

    list(statistic = stat, p_value = pval, df = df_num)
  }

  groups <- unique(data_full$wb_income_group)
  cat("\nGrupos de renda: ", paste(groups, collapse = ", "), "\n")

  for (g in groups) {
    cat("\n--- Estimando modelo para grupo:", g, "---\n")

    df <- data_full %>% filter(wb_income_group == g)

    # Requisitos mínimos para GMM
    if (length(unique(df$year)) < 6) {
      cat("T muito pequeno para GMM. Pulando.\n")
      next
    }

    if (length(unique(df$iso3c)) < 5) {
      cat("Poucos países neste grupo. Pulando.\n")
      next
    }

    panel_df <- pdata.frame(df, index = c("iso3c", "year"))

    model_variant <- NA_character_

    # Primeiro tenta System GMM one-step
    out <- try(
      pgmm(
        formula = model_formula,
        data = panel_df,
        effect = "twoways",
        model = "onestep",
        transformation = "ld",
        collapse = TRUE
      ),
      silent = TRUE
    )

    if (!inherits(out, "try-error")) {
      model_variant <- "system_ld"
    }

    if (inherits(out, "try-error")) {
      cat("System GMM falhou. Tentando Difference GMM...\n")

      out <- try(
        pgmm(
          formula = model_formula,
          data = panel_df,
          effect = "twoways",
          model = "onestep",
          transformation = "d",
          collapse = TRUE
        ),
        silent = TRUE
      )

      if (!inherits(out, "try-error")) {
        model_variant <- "difference_d"
      }
    }

    if (inherits(out, "try-error")) {
      cat("Não foi possível estimar nenhum modelo GMM para este grupo.\n")
    } else {
      summ <- summary(out, robust = FALSE)
      print(summ)

      hansen_res <- report_hansen_j(out)
      if (is.null(hansen_res)) {
        hansen_res <- list(statistic = NA_real_, p_value = NA_real_, df = NA_real_)
      }

      wu_res <- run_wu_hausman(df, wu_formula)

      if (is.null(wu_res) || is.na(wu_res$statistic)) {
        cat("Teste Wu-Hausman: não foi possível calcular para este grupo.\n")
        wu_out <- list(statistic = NA_real_, p_value = NA_real_, df1 = NA_real_, df2 = NA_real_)
      } else {
        df1_txt <- ifelse(is.na(wu_res$df1), "-", formatC(wu_res$df1, format = "f", digits = 0))
        df2_txt <- ifelse(is.na(wu_res$df2), "-", formatC(wu_res$df2, format = "f", digits = 0))
        cat(
          sprintf(
            "Teste Wu-Hausman F: estatística = %.3f (df1=%s, df2=%s), p-valor = %.4f\n",
            wu_res$statistic,
            df1_txt,
            df2_txt,
            wu_res$p_value
          )
        )
        wu_out <- wu_res
      }

      coef_mat <- summ$coefficients
      if (!is.null(coef_mat) && nrow(coef_mat) > 0) {
        coef_df <- data.frame(
          term = rownames(coef_mat),
          coef_mat,
          row.names = NULL,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )

        meta_df <- data.frame(
          income_group = g,
          model_variant = model_variant,
          hansen_stat = hansen_res$statistic,
          hansen_df = hansen_res$df,
          hansen_p_value = hansen_res$p_value,
          wu_statistic = wu_out$statistic,
          wu_df1 = wu_out$df1,
          wu_df2 = wu_out$df2,
          wu_p_value = wu_out$p_value,
          stringsAsFactors = FALSE
        )

        meta_df <- meta_df[rep(1, nrow(coef_df)), , drop = FALSE]
        income_results[[length(income_results) + 1]] <- cbind(meta_df, coef_df)

        income_tests[[length(income_tests) + 1]] <- data.frame(
          income_group = g,
          model_variant = model_variant,
          hansen_stat = hansen_res$statistic,
          hansen_df = hansen_res$df,
          hansen_p_value = hansen_res$p_value,
          wu_statistic = wu_out$statistic,
          wu_df1 = wu_out$df1,
          wu_df2 = wu_out$df2,
          wu_p_value = wu_out$p_value,
          stringsAsFactors = FALSE
        )
      }
    }
  }

  if (length(income_results) > 0) {
    income_output <- file.path(reports_dir, "gmm_system_models_income.csv")
    income_results_df <- do.call(rbind, income_results)
    write.csv(income_results_df, income_output, row.names = FALSE)
    cat(sprintf("\nResultados tabulados salvos em %s\n", income_output))
  } else {
    cat("\nNenhum resultado para exportar em gmm_system_models_income.\n")
  }

  if (length(income_tests) > 0) {
    income_tests_output <- file.path(reports_dir, "gmm_system_models_income_tests.csv")
    income_tests_df <- do.call(rbind, income_tests)
    write.csv(income_tests_df, income_tests_output, row.names = FALSE)
    cat(sprintf("Resultados dos testes salvos em %s\n", income_tests_output))
  }

  cat("\n--- Análise concluída. ---\n")
})
