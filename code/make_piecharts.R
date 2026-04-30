library(ggplot2)
library(dplyr)
library(patchwork)

# ---------- DATI ----------
df1 <- data.frame(
  category = c("lost", "acquired", "changed", "conserved"),
  value = c(5324, 1891, 90, 343)
)

df2 <- data.frame(
  category = c("lost", "acquired", "changed", "conserved"),
  value = c(831, 283, 20, 75)
)

df3 <- data.frame(
  category = c("acquired", "conserved", "lost"),
  value = c(170, 39, 91)
)

# ---------- COLORI ----------
colors <- c(
  "acquired" = "#D084F5",   # viola
  "changed"  = "#F5F553",   # giallo
  "conserved"= "#2ECC71",   # verde
  "lost"     = "#00E5FF"    # azzurro
)

# ---------- FUNZIONE PER PLOT A TORTA ----------
pie_plot <- function(data, title) {
  data %>%
    mutate(
      pct = round(value / sum(value) * 100),
      label = paste0(value, "; ", pct, "%")
    ) %>%
    ggplot(aes(x = "", y = value, fill = category)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y") +
    geom_text(aes(label = label),
              position = position_stack(vjust = 0.5),
              size = 3) +
    scale_fill_manual(values = colors) +
    theme_void() +
    ggtitle(title) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
      legend.position = "none"
    )
}

# ---------- COSTRUZIONE DEI TRE GRAFICI ----------
p1 <- pie_plot(df1, "Apollo 12 regolith vs JSC-1A")
p2 <- pie_plot(df2, "Apollo 17 regolith vs JSC-1A")
p3 <- pie_plot(df3, "Apollo 12 vs Apollo 17 regolith")

# ---------- LEGGENDA SEPARATA ----------
legend_plot <- ggplot(df1, aes(x="", y=value, fill=category)) +
  geom_col() +
  scale_fill_manual(values=colors, name="") +
  theme_void() +
  theme(legend.text = element_text(size=8))

legend <- cowplot::get_legend(legend_plot)

# ---------- PANNELO FINALE ----------
(p1 | p2 | p3) / legend
