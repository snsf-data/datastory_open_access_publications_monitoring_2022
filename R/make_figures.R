if (interactive()) {
  
  source("R/read_and_prepare_data.R")
  params <- NULL
  params$lang <- "en"
  source("R/translations.R")
  
  library(ggplot2)
  library(ggiraph)
  library(scales)
  library(glue)
  
  if (!require("snf.datastory")) {
    if (!require("devtools")) {
      install.packages("devtools")
      library(devtools)
    }
    install_github("snsf-data/snf.datastory")
    library(snf.datastory)
  }
  
  font <- "sans"
  
}

#==============================================================================#
# Figure 1: evolution of OA shares per OA category since 2013               ####
#==============================================================================#

make_figure_1 <- function() {
  
  all_periods_palette_fun <- colorRampPalette(c("#A8DEF8", "#0C293C"))
  
  plot <-
    former_monitorings |>
    mutate(
      category =
        case_when(
          category == "SNSF OA (gold, green, hybrid)" ~ oa_snsf,
          category == "other OA" ~ oa_other,
          category == "restricted" ~ oa_restricted
        ) |>
        # Ordering and labels
        fct_relevel(
          c(oa_snsf, oa_other, oa_restricted)
        ),
      data_id = row_number()
    ) |>
    ggplot() +
    aes(
      x = category,
      y = value,
      fill = period,
      tooltip =
        paste0(
          switch(
            params$lang,
            en = "OA category: ",
            de = "OA-Kategorie: ",
            fr = "Catégorie OA : "
          ), category, "<br>",
          switch(
            params$lang,
            en = "Period: ",
            de = "Zeitraum: ",
            fr = "Période: "
          ), period, "<br>",
          switch(
            params$lang,
            en = "Publication percentage: ",
            de ="Publikationen in Prozent: ",
            fr = "Pourcentage de publications : "
          ), value, "%"
        ),
      data_id = data_id
    ) +
    # Hack: Add a geom_col under the interactive one, only to be able to provide
    # correct looking legend items (round although bar chart),
    # geom_col_interactive does not take the argument 'key_glyph'
    geom_col(
      position = position_dodge(width = 0.8),
      width = 0.1,
      # Draw point instead of square symbol
      key_glyph = draw_key_dotplot
    ) +
    geom_col_interactive(
      position = position_dodge(width = 0.8),
      width = 0.8,
      color = "white",
      show.legend = FALSE
    ) +
    geom_text(
      aes(label = paste0(value, "%"), y = (value - 6)),
      hjust = 0.5,
      color = "white",
      fontface = "bold",
      family = font,
      position = position_dodge(width = 0.8),
      size = 2.75
    ) +
    scale_y_continuous(
      labels = percent_format(accuracy = 1, scale = 1),
      limits = c(0, 85), breaks = seq(0, 85, 10)
    ) +
    scale_fill_manual(
      values =
        all_periods_palette_fun(length(unique(former_monitorings$period)))
    ) +
    labs(x = NULL, y = NULL) +
    get_datastory_theme(text_axis = "x", family = font) +
    theme(
      legend.text = element_text(size = 10),
      axis.text.x = element_text(size = 10)
    ) +
    guides(fill = guide_legend(nrow = 1))
  
  make_ggiraph(plot, h = 3.5, sw = NA, scolor = NA)
  
}

#==============================================================================#
# Figure 2: shares per OA category in 2022                                  ####
#==============================================================================#

make_figure_2 <- function() {
  
  oa_colors <- c("#406AF5", "#90AAFF", "#CCD8FF", "#AFAFAF", "#4F4F4F")
  names(oa_colors) <- c(oa_gold, oa_green, oa_hybrid, oa_other, oa_restricted)
  
  plot <-
    snsf_oa_categories_2022 |>
    mutate(
      oa_status = fct_relabel(
        oa_status,
        \(x)
        case_when(
          x == "gold" ~ oa_gold,
          x == "green" ~ oa_green,
          x == "hybrid" ~ oa_hybrid,
          x == "other OA" ~ oa_other,
          x == "restricted" ~ oa_restricted
        )
      )
    ) |>
    ggplot() +
    aes(
      x = fct_rev(oa_status),
      y = freq,
      fill = oa_status,
      data_id = data_id,
      tooltip =
        paste0(
          switch(
            params$lang,
            en = "OA category: ", de = "OA-Kategorie: ", fr = "Catégorie OA : "
          ), oa_status, "<br>",
          switch(
            params$lang,
            en = "Publication percentage: ",
            de ="Publikationen in Prozent: ",
            fr = "Pourcentage de publications : "
          ),
          print_num(round(freq * 100, 1)), "%<br>",
          switch(
            params$lang,
            en = "Number of publications in 2022: ",
            de = "Anzahl Publikationen 2022: ",
            fr = "Nombre de publications en 2022 : "
          ),
          print_num(n)
        )
    ) +
    geom_col_interactive(width = 0.8) +
    geom_text(
      aes(
        y = if_else(oa_status == oa_gold, freq - 0.04, freq - 0.02),
        label = paste0(round(freq * 100), "%")
      ),
      size = 3.5,
      hjust = 0.5,
      family = font,
      color = "white",
      fontface = "bold"
    ) +
    geom_point_interactive(
      data =
        snsf_oa_categories_2021 |>
        mutate(
          oa_status = fct_relabel(
            oa_status,
            \(x)
            case_when(
              x == "gold" ~ oa_gold,
              x == "green" ~ oa_green,
              x == "hybrid" ~ oa_hybrid,
              x == "other OA" ~ oa_other,
              x == "restricted" ~ oa_restricted
            )
          )
        ),
      aes(
        tooltip =
          paste0(
            switch(
              params$lang,
              en = "OA category: ",
              de = "OA-Kategorie: ",
              fr = "Catégorie OA : "
            ), oa_status, "<br>",
            switch(
              params$lang,
              en = "Publication percentage: ",
              de ="Publikationen in Prozent: ",
              fr = "Pourcentage de publications : "
            ),
            print_num(round(freq * 100, 1)), "%<br>",
            switch(
              params$lang,
              en = "Number of publications in 2021: ",
              de = "Anzahl Publikationen 2021: ",
              fr = "Nombre de publications en 2021 : "
            ),
            print_num(n)
          ),
        fill = NA,
        color = "#B2B1A7",
        data_id = data_id
      ),
      size = 2, stroke = 1, color = "transparent",
      shape = 21
    ) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    scale_fill_manual(values = oa_colors, guide = "none") +
    coord_flip() +
    get_datastory_theme(family = font) +
    theme(
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10)
    )
  
  make_ggiraph(plot, h = 3.5, sw = NA, scolor = NA)
  
}

#==============================================================================#
# Figure 3: Shares of licenses in 2022                                      ####
#==============================================================================#

make_figure_3 <- function() {
  
  plot <-
    bind_rows(
      license_dat_2022 |>
        summarise(
          n = sum(n),
          .by = license
        ) |>
        mutate(
          pct = n / sum(n),
          monitoring = 2022
        ),
      license_dat_2021 |>
        summarise(
          n = sum(n),
          .by = license
        ) |>
        mutate(
          pct = n / sum(n),
          monitoring = 2021
        )
    ) |>
    mutate(
      license = fct_relabel(
        license,
        \(x)
        case_when(
          x == "Unknown" ~ lc_unknown,
          x == "Other CC BY" ~ lc_other_cc_by,
          x == "Other OA/public domain" ~ lc_other_oa,
          .default = x
        )
      ),
      license = fct_reorder(license, n, .desc = TRUE)
    ) |>
    mutate(
      diff = pct[monitoring == 2022] - pct[monitoring == 2021],
      .by = license
    ) |>
    mutate(
      data_id = row_number(),
    ) |>
    ggplot() +
    aes(
      x = pct,
      y = license,
      fill = license,
      data_id = data_id,
      tooltip =
        paste0(
          switch(
            params$lang,
            en = "Licence: ",
            de = "Lizenz: ",
            fr = "Licence : "
          ), license, "<br>",
          switch(
            params$lang,
            en = "Publication percentage: ",
            de ="Publikationen in Prozent: ",
            fr = "Pourcentage de publications : "
          ),
          print_num(round(pct * 100, 1)), "%<br>",
          switch(
            params$lang,
            en = "Number of publications in 2022: ",
            de = "Anzahl Publikationen 2022: ",
            fr = "Nombre de publications en 2022 : "
          ),
          print_num(n)
        )
    ) +
    geom_col_interactive(
      data = \(x) filter(x, monitoring == 2022),
      show.legend = FALSE
    ) +
    geom_point_interactive(
      data = \(x) filter(x, monitoring == 2021),
      aes(
        tooltip =
          paste0(
            switch(
              params$lang,
              en = "Licence: ",
              de = "Lizenz: ",
              fr = "Licence : "
            ), license, "<br>",
            switch(
              params$lang,
              en = "Publication percentage: ",
              de ="Publikationen in Prozent: ",
              fr = "Pourcentage de publications : "
            ),
            print_num(round(pct * 100, 1)), "%<br>",
            switch(
              params$lang,
              en = "Number of publications in 2021: ",
              de = "Anzahl Publikationen 2021: ",
              fr = "Nombre de publications en 2021 : "
            ),
            print_num(n)
          ),
        fill = NA,
        color = "#B2B1A7",
        data_id = data_id
      ),
      size = 2,
      stroke = 1,
      color = "transparent",
      shape = 21,
      show.legend = FALSE
    ) +
    geom_text(
      data = \(x) filter(x, monitoring == 2022),
      aes(
        label =
          paste0(
            if_else(
              round(pct * 100) == 0 & n > 0,
              "< 1", as.character(round(pct * 100))
            )
            , "%"
          ),
        x =
          case_when(
            pct > 0.03 & diff < 0 ~ pct - 0.00375,
            pct > 0.03 & abs(diff) > 0.05 ~ pct - 0.00375,
            abs(diff) < 0.05 & diff < 0 ~ pct + 0.00375 * 3,
            .default = pct + 0.00375
          ),
        color =
          case_when(
            pct > 0.03 & diff < 0 ~ "white",
            pct > 0.03 & abs(diff) > 0.05 ~ "white",
            abs(diff) < 0.05 & diff < 0 ~ "#4F4F4F",
            .default = "#4F4F4F"
          ),
        hjust =
          case_when(
            pct > 0.03 & diff < 0 ~ 1,
            pct > 0.03 & abs(diff) > 0.05 ~ 1,
            .default = 0
          )
      ),
      size = 3.25,
      family = font,
      fontface = "bold",
      show.legend = FALSE
    ) +
    scale_fill_manual(values = get_datastory_scheme(n_col = 7)[-4]) +
    scale_x_continuous(
      labels = scales::percent, 
      expand = expansion(add = c(0, 0.05))
    ) +
    scale_color_identity() +
    get_datastory_theme(text_axis = "x", family = font) +
    facet_wrap(~license, ncol = 1, scales = "free_y") +
    theme(
      axis.text.x = element_text(size = 10),
      legend.text = element_text(size = 10),
      strip.text =
        element_text(
          face = "plain",
          size = 10,
          hjust = 0,
          color = "#4F4F4F",
          margin = margin(1, 1, 1)
        ),
      panel.spacing.y = unit(0.25, "lines")
    )
  
  make_ggiraph(plot, h = 4, sw = NA, scolor = NA)
  
}

#==============================================================================#
# Figure 4: Shares of licenses par OA type in 2022                          ####
#==============================================================================#

make_figure_4 <- function() {
  
  plot <-
    license_dat_2022 |>
    mutate(
      license =
        fct_relevel(license, "Unknown", "Other OA/public domain") |>
        fct_relabel(
          \(x)
          case_when(
            x == "Unknown" ~ lc_unknown,
            x == "Other CC BY" ~ lc_other_cc_by,
            x == "Other OA/public domain" ~ lc_other_oa,
            .default = x
          )
        ),
      oa_status =
        fct_relabel(
          oa_status,
          \(x) case_when(
            x == "Gold" ~ oa_gold,
            x == "Green" ~ oa_green,
            x == "Hybrid" ~ oa_hybrid,
            x == "Other OA" ~ oa_other,
            x == "Restricted" ~ oa_restricted
          )
        ),
      data_id = row_number(),
      label = paste0(round(pct * 100), "%")
    ) |>
    arrange(license) |>
    mutate(
      text_pos =  1 - (cumsum(dplyr::lag(pct, default = 0)) + pct / 2),
      .by = oa_status
    ) |>
    ggplot() +
    aes(
      x = pct,
      y = oa_status,
      fill = license,
      label = label,
      tooltip =
        paste0(
          switch(
            params$lang,
            en = "Licence: ",
            de = "Lizenz: ",
            fr = "Licence : "
          ), license, "<br>",
          switch(
            params$lang,
            en = "OA category: ",
            de = "OA-Kategorie: ",
            fr = "Catégorie OA : "
          ), oa_status, "<br>",
          switch(
            params$lang,
            en = "Publication percentage: ",
            de ="Publikationen in Prozent: ",
            fr = "Pourcentage de publications : "
          ), paste0(round(pct * 100), "%"), "<br>",
          switch(
            params$lang,
            en = "Number of publications in 2022: ",
            de = "Anzahl Publikationen 2022: ",
            fr = "Nombre de publications en 2022 : "
          ), n
        ),
      data_id = data_id
    ) +
    geom_col_interactive() +
    geom_text(
      aes(
        color = if_else(pct < 0.06, "transparent", "white"),
        x = text_pos
      ),
      size = 3.25,
      hjust = 0.5,
      family = font,
      fontface = "bold"
    ) +
    # To match the color of the previous figure
    scale_fill_manual(
      values =
        c(
          get_datastory_scheme()[2],
          get_datastory_scheme()[6],
          get_datastory_scheme()[7],
          get_datastory_scheme()[5],
          get_datastory_scheme()[3],
          get_datastory_scheme()[1]
        )
    ) +
    scale_x_continuous(
      labels = scales::percent,
      expand = expansion(add = c(0, 0.05))
    ) +
    scale_color_identity() +
    get_datastory_theme(text_axis = "x", family = font) +
    facet_wrap(~oa_status, ncol = 1, scales = "free_y") +
    theme(
      axis.text.x = element_text(size = 10),
      legend.text = element_text(size = 10),
      strip.text =
        element_text(
          face = "plain",
          size = 10,
          hjust = 0,
          color = "#4F4F4F",
          margin = margin(1, 1, 1)
        ),
      panel.spacing.y = unit(0.25, "lines")
    ) +
    guides(fill = guide_legend(reverse = TRUE, byrow = TRUE))
  
  make_ggiraph(plot, h = 4, sw = NA, scolor = NA)
  
}

#==============================================================================#
# Others                                                                    ####
#==============================================================================#

make_ggiraph <- function(x,                    # ggplot object
                         h = 4,                # height of the svg generated
                         sw = 2,               # width of the stroke
                         fcolor = "#f6685e",   # color (fill)
                         color = NA,           # color
                         scolor = "#f6685e") { # color of the stroke
  
  girafe(
    ggobj = x,
    height_svg = h,
    options = list(
      opts_toolbar(saveaspng = FALSE),
      opts_hover(
        css =
          girafe_css(
            css =
              glue(
                "fill:{fcolor};color:{color};stroke:{scolor};stroke-width:{sw};"
              ),
            text = "stroke:none; color:blue;"
          )
      ),
      opts_tooltip(
        css = get_ggiraph_tooltip_css(family = font),
        opacity = 0.8,
        delay_mouseover = 0,
        delay_mouseout = 0
      )
    )
  )
}
