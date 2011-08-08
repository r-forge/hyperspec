
    pse <- data.frame (Z = 1 : 18,
                       symbol = c (
                         "H", "He",
                         "Li", "Be", "B", "C", "N", "O", "F", "Ne",
                         "Na", "Mg", "Al", "Si", "P", "S", "Cl", "Ar"),
                       configuration = factor (
                         c ("s^1", "s^2",
                            "s^1", "s^2", "p^1", "p^2", "p^3", "p^4", "p^5", "p^6",
                            "s^1", "s^2", "p^1", "p^2", "p^3", "p^4", "p^5", "p^6"
                            ),
                         c (paste ("s", 1 : 2, sep = "^"),
                            "d^1",
                            paste ("f", 1 : 14, sep = "^"),
                            paste ("d", 2 : 10, sep = "^"),
                            paste ("p", 1 : 6, sep = "^")
                            )),
                       group = c (1, 18, 1 : 2, 13 : 18, 1 : 2, 13 : 18),
                       period = c (rep (1, 2), rep (2, 8), rep (3, 8))
                       )
    
    grp <- c ("IA", "IIA",
              "IIIB", "IVB", "VB", "VIB", "VIIB", "VIIIB", "VIIIB", "VIIIB", "IB", "IIB",
              "IIIA", "IVA", "VA", "VIA", "VIIA", "VIIIA")
    
    library (ggplot2)
    ggplot (pse, aes (x = group, y = period)) +
      geom_text (aes (label = symbol)) +
      ylim (rev (range (pse$period))) +
      scale_x_continuous (breaks = 1 : 18, labels = grp)
    
    ggplot (pse, aes (x = configuration, y = period)) +
      geom_text (aes (label = symbol)) +
      ylim (rev (range (pse$period))) +
      scale_x_discrete (formatter = function (x) parse (text = x))
    
    
