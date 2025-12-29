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
      mutate(
        break_pandemic = as.integer(year >= 2020),
        break_commod = as.integer(year >= 2014 & year <= 2016),
        break_2022 = as.integer(year >= 2022)
      ) %>%
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
    log_edu + ef | lag(log_gdp,2:5) + lag(log_inv,2:6) + lag(log_lab_part,2:5) +
    lag(log_edu,2:5) + break_pandemic + break_commod + break_2022

  cat("Usando fórmula GMM: ", deparse(model_formula), "\n")

  run_wu_hausman <- function(df, model_formula) {
    fstr <- paste(deparse(model_formula), collapse = " ")
    parts <- strsplit(fstr, "\\|")[[1]]
    if (length(parts) < 2) return(NULL)

    left <- trimws(parts[1])
    instr <- trimws(parts[2])

    left <- gsub("lag\\(\\s*log_gdp\\s*,\\s*1\\s*\\)", "log_gdp_lag1", left)
    instr <- gsub("lag\\(\\s*log_gdp\\s*,\\s*2\\s*:\\s*3\\s*\\)", "log_gdp_lag2 + log_gdp_lag3", instr)
    instr <- gsub("lag\\(\\s*log_gdp\\s*,\\s*2\\s*\\)", "log_gdp_lag2", instr)
    instr <- gsub("lag\\(\\s*log_gdp\\s*,\\s*3\\s*\\)", "log_gdp_lag3", instr)

    iv_formula_str <- paste(left, "|", instr)
    iv_formula <- try(as.formula(iv_formula_str), silent = TRUE)
    if (inherits(iv_formula, "try-error")) return(NULL)

    iv_fit <- try(AER::ivreg(formula = iv_formula, data = df), silent = TRUE)
    if (inherits(iv_fit, "try-error")) return(NULL)

    diagnostics <- summary(iv_fit, diagnostics = TRUE)$diagnostics
    if (is.null(diagnostics)) return(NULL)

    label <- if ("Wu-Hausman F" %in% rownames(diagnostics)) {
      "Wu-Hausman F"
    } else if ("Wu-Hausman" %in% rownames(diagnostics)) {
      "Wu-Hausman"
    } else {
      NULL
    }

    if (is.null(label)) return(NULL)

    stats <- diagnostics[label, , drop = FALSE]
    pull_value <- function(col) { if (col %in% colnames(stats)) stats[1, col] else NA_real_ }

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
      cat("Hansen J-test (via sargan): não foi possível calcular para este grupo.\n")
      return(list(statistic = NA_real_, p_value = NA_real_, df = NA_real_, test = NA_character_))
    }

    stat <- ifelse(is.null(hansen$statistic), NA_real_, as.numeric(hansen$statistic))
    pval <- ifelse(is.null(hansen$p.value), NA_real_, as.numeric(hansen$p.value))
    df_num <- ifelse(is.null(hansen$parameter), NA_real_, as.numeric(hansen$parameter))

    if (is.na(stat) || is.na(pval)) {
      cat("Hansen J-test (via sargan): não foi possível calcular para este grupo.\n")
      return(list(statistic = NA_real_, p_value = NA_real_, df = df_num, test = "Sargan"))
    }

    df_txt <- ifelse(is.na(df_num), "-", formatC(df_num, format = "f", digits = 0))
    cat(sprintf("Hansen/Sargan J-test: J = %.3f (df=%s), p-valor = %.4f\n", stat, df_txt, pval))

    list(statistic = stat, p_value = pval, df = df_num, test = "Sargan")
  }

  get_overid_test <- function(model) {
    # Tenta extrair teste robusto (Hansen) via summary, depois faz fallback para sargan()
    summ_r <- tryCatch(summary(model, robust = TRUE), error = function(e) NULL)
    if (!is.null(summ_r) && !is.null(summ_r$diagnostics)) {
      diag <- summ_r$diagnostics
      rn <- rownames(diag)
      idx <- which(grepl("Hansen", rn, ignore.case = TRUE) | grepl("J-test", rn, ignore.case = TRUE) | grepl("J", rn))
      if (length(idx) > 0) {
        stat <- suppressWarnings(as.numeric(diag[idx[1], "statistic"]))
        pval <- suppressWarnings(as.numeric(diag[idx[1], "p-value"]))
        return(list(test = "Hansen", statistic = stat, p_value = pval, df = NA_real_, robust = TRUE))
      }
    }

    s <- tryCatch(sargan(model), error = function(e) NULL)
    if (!is.null(s)) {
      stat <- ifelse(is.null(s$statistic), NA_real_, as.numeric(s$statistic))
      pval <- ifelse(is.null(s$p.value), NA_real_, as.numeric(s$p.value))
      df_num <- ifelse(is.null(s$parameter), NA_real_, as.numeric(s$parameter))
      return(list(test = "Sargan", statistic = stat, p_value = pval, df = df_num, robust = FALSE))
    }

    list(test = NA_character_, statistic = NA_real_, p_value = NA_real_, df = NA_real_, robust = NA)
  }

  run_all_tests <- function(model, df, model_formula) {
    overid <- get_overid_test(model)

    ar2_test <- tryCatch(plm::mtest(model, order = 2), error = function(e) NULL)
    if (is.null(ar2_test)) {
      ar2_out <- list(statistic = NA_real_, p_value = NA_real_)
    } else {
      ar2_stat <- suppressWarnings(as.numeric(ar2_test$statistic[1]))
      ar2_p <- suppressWarnings(as.numeric(ar2_test$p.value))
      ar2_out <- list(statistic = ar2_stat, p_value = ar2_p)
    }

    wu_res <- run_wu_hausman(df, model_formula)

    list(overid = overid, ar2 = ar2_out, wu = wu_res)
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

      tests <- run_all_tests(out, df, model_formula)
      overid <- tests$overid
      ar2_out <- tests$ar2
      wu_out <- tests$wu

      if (is.null(overid) || is.na(overid$statistic)) {
        cat("Teste de sobreidentificação: não foi possível calcular para este grupo.\n")
        hansen_res <- list(statistic = NA_real_, p_value = NA_real_, df = NA_real_, test = NA_character_)
      } else {
        hansen_res <- list(statistic = overid$statistic, p_value = overid$p_value, df = overid$df, test = overid$test)
      }

      if (is.null(ar2_out) || is.na(ar2_out$statistic)) {
        cat("Teste Arellano-Bond AR(2): não foi possível calcular para este grupo.\n")
      } else {
        cat(sprintf("Teste Arellano-Bond AR(2): estatística = %.3f, p-valor = %.4f\n", ar2_out$statistic, ar2_out$p_value))
      }

      if (is.null(wu_out) || is.na(wu_out$statistic)) {
        cat("Teste Wu-Hausman: não foi possível calcular para este grupo.\n")
        wu_out <- list(statistic = NA_real_, p_value = NA_real_, df1 = NA_real_, df2 = NA_real_)
      } else {
        df1_txt <- ifelse(is.na(wu_out$df1), "-", formatC(wu_out$df1, format = "f", digits = 0))
        df2_txt <- ifelse(is.na(wu_out$df2), "-", formatC(wu_out$df2, format = "f", digits = 0))
        cat(sprintf("Teste Wu-Hausman F: estatística = %.3f (df1=%s, df2=%s), p-valor = %.4f\n", wu_out$statistic, df1_txt, df2_txt, wu_out$p_value))
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
          ar2_statistic = ar2_out$statistic,
          ar2_p_value = ar2_out$p_value,
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
          ar2_statistic = ar2_out$statistic,
          ar2_p_value = ar2_out$p_value,
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
