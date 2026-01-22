library(ggplot2)

# ----- Settings -----
n <- 10
x <- 0:10
eps <- 1e-10

# ----- Binomial probability -----
bin <- function(k, p) {
  choose(n, k) * p^k * (1 - p)^(n - k)
}

Bin <- function(p) {
  sapply(0:10, function(k) bin(k, p))
}

# ----- Plot Binomial -----
plot_Bin <- function(p) {
  df <- data.frame(
    s = x,
    P = Bin(p)
  )
  ggplot(df, aes(x = s, y = P)) +
    geom_bar(stat = "identity", fill = "skyblue") +
    labs(
      x = "s",
      y = bquote(P(s) == Bin[.(n), .(p)](s)),
      title = paste("The probability distribution of the Krapfen-score assuming p =", p)
    ) +
    theme_minimal()
}

# ----- Posterior helper -----
test_post <- function(p_arr) {
  stopifnot(abs(sum(p_arr) - 1) < eps)
}

plot_post <- function(p_arr) {
  test_post(p_arr)
  df <- data.frame(s = x, P = p_arr)
  ggplot(df, aes(x = s, y = P)) +
    geom_bar(stat = "identity", fill = "orange") +
    labs(
      x = "s",
      y = expression(P~tilde(s)),
      title = "The a posteriori probability distribution of the Krapfen-score"
    ) +
    theme_minimal()
}

# ----- Maximum likelihood estimate -----
ML_estimate <- function(survey_data) {
  sum(survey_data) / (length(survey_data) * n)
}

# ----- A posteriori probability function -----
get_a_posteriori <- function(survey_data, expert_opinion) {
  mle <- ML_estimate(survey_data)
  s1 <- expert_opinion[1]
  s2 <- expert_opinion[2]
  
  a_posteriori <- function(s) {
    # Probability of event given s
    p_judith_s1_given_s <- bin(s1, s / 10)
    p_alex_s2_given_s <- bin(s2, s / 10)
    p_event_given_s <- p_judith_s1_given_s * p_alex_s2_given_s
    
    # Prior for s
    p_s <- bin(s, mle)
    
    # Probability of event
    p_person_s_given_i <- function(s, i) bin(s, i / 10)
    p_i <- function(i) bin(i, mle)
    p_event <- sum(sapply(0:10, function(i) {
      p_person_s_given_i(s1, i) * p_person_s_given_i(s2, i) * p_i(i)
    }))
    
    # Posterior
    p_event_given_s * (p_s / p_event)
  }
  
  return(a_posteriori)
}

# ----- Expected value -----
expected_value <- function(p_arr) {
  test_post(p_arr)
  sum(x * p_arr)
}

# ----- Comparison plot -----
plot_comparison <- function(survey_data, expert_opinion) {
  mle <- ML_estimate(survey_data)
  prio <- Bin(mle)
  
  a_post <- get_a_posteriori(survey_data, expert_opinion)
  post <- sapply(x, a_post)
  
  mle_expert <- ML_estimate(expert_opinion)
  expe <- Bin(mle_expert)
  
  data_list <- list(prio = prio, post = post, expe = expe)
  df <- rbindlist(lapply(names(data_list), function(name) {
    data.frame(
      Score = x,
      Probability = data_list[[name]],
      Distribution = name,
      Title = switch(
        name,
        prio = paste0("A priori distribution based on surveys\nExpected Score = ", round(expected_value(data_list[[name]]), 3)),
        post = paste0("A posteriori distribution\nExpected Score = ", round(expected_value(data_list[[name]]), 3)),
        expe = paste0("Distribution based only on expert opinion (Values :", paste(expert_opinion, collapse = ", "),
                      ")\nExpected Score = ", round(expected_value(data_list[[name]]), 3))
      )
    )
  }))
  # Plot using ggplot with facets
  df[, Distribution := factor(Distribution, levels = c("post", "prio", "expe"))]
  ggplot(df, aes(x = Score, y = Probability, fill = Distribution, color = Distribution)) +
    geom_bar(stat = "identity", show.legend = FALSE) +
    facet_wrap(~Title, nrow = 1) +
    scale_fill_manual(values = c(prio = "#f5a6d2", post = "#EC0B88", expe = "#3d98d3")) +
    scale_color_manual(values = c(prio = "#f5a6d2", post = "#EC0B88", expe = "#3d98d3")) +
    labs(
      x = "Score",
      y = "Probability"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      strip.text = element_text(size = 14),
      axis.text.x = element_text(angle = 0, hjust = 0.5)
    )
}

get_viable_krapfen_options <- function(expert_data) {
  expert_data <- expert_data[, lapply(.SD, function(col) {
    if (any(is.na(col))) {
      NULL
    } else {
      col
    }
  })]
  k_names <- colnames(expert_data)[-1]
  return(k_names)
}


# ----- Example usage -----
# sample_survey_data <- c(9, 8, 9, 9, 9)
# sample_expert_opinion <- c(5, 7)
# plot_comparison(sample_survey_data, sample_expert_opinion)
  