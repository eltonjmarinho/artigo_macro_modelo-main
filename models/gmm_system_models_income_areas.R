# --- Estimação de Modelos GMM Sistêmicos por Grupo de Renda com Subcomponentes de Liberdade ---

# Suppress warnings immediately (incluindo carregamento de pacotes); restaura ao sair
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

  area_results <- list()
  area_tests <- list()

  area_vars <- c("area1", "area2", "area3", "area4", "area5")
  area_labels <- c(
    area1 = "Area 1 Size of Government",
    area2 = "Area 2 Legal System & Property Rights (ajustada por gênero)",
    area3 = "Area 3 Sound Money",
    area4 = "Area 4 Freedom to trade internationally",
    area5 = "Area 5 Regulation"
  )

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
  cat("\nGrupos de renda: ", paste(groups, collapse = ", "), "\n", sep = "")

  for (area_var in area_vars) {
    cat(sprintf("\n=== Resultados usando %s (%s) ===\n", area_labels[[area_var]], area_var))

    gmm_formula <- as.formula(sprintf(
      "log_gdp ~ lag(log_gdp, 1) + log_inv + log_lab_part + log_edu + %s | lag(log_gdp, 2:3)",
      area_var
    ))

    wu_formula <- as.formula(sprintf(
      "log_gdp ~ log_gdp_lag1 + log_inv + log_lab_part + log_edu + %s | log_gdp_lag2 + log_gdp_lag3",
      area_var
    ))

    for (g in groups) {
      cat("\n--- Estimando modelo para grupo:", g, "---\n")

      df <- data_full %>%
        filter(
          wb_income_group == g,
          is.finite(.data[[area_var]])
        )

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

      out <- try(
        pgmm(
          formula = gmm_formula,
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
            formula = gmm_formula,
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

        ar2_test <- tryCatch(plm::mtest(out, order = 2), error = function(e) NULL)
        if (is.null(ar2_test)) {
          cat("Teste Arellano-Bond AR(2): não foi possível calcular para este grupo.\n")
          ar2_out <- list(statistic = NA_real_, p_value = NA_real_)
        } else {
          ar2_stat <- suppressWarnings(as.numeric(ar2_test$statistic[1]))
          ar2_p <- suppressWarnings(as.numeric(ar2_test$p.value))
          cat(sprintf("Teste Arellano-Bond AR(2): estatística = %.3f, p-valor = %.4f\n",
                      ar2_stat, ar2_p))
          ar2_out <- list(statistic = ar2_stat, p_value = ar2_p)
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
            area_var = area_var,
            area_label = area_labels[[area_var]],
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
          area_results[[length(area_results) + 1]] <- cbind(meta_df, coef_df)

          area_tests[[length(area_tests) + 1]] <- data.frame(
            area_var = area_var,
            area_label = area_labels[[area_var]],
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
  }

  if (length(area_results) > 0) {
    area_output <- file.path(reports_dir, "gmm_system_models_income_areas.csv")
    area_results_df <- do.call(rbind, area_results)
    write.csv(area_results_df, area_output, row.names = FALSE)
    cat(sprintf("\nResultados tabulados salvos em %s\n", area_output))
  } else {
    cat("\nNenhum resultado para exportar em gmm_system_models_income_areas.\n")
  }

  if (length(area_tests) > 0) {
    area_tests_output <- file.path(reports_dir, "gmm_system_models_income_areas_tests.csv")
    area_tests_df <- do.call(rbind, area_tests)
    write.csv(area_tests_df, area_tests_output, row.names = FALSE)
    cat(sprintf("Resultados dos testes salvos em %s\n", area_tests_output))
  }

  cat("\n--- Análise GMM por áreas concluída. ---\n")
})
