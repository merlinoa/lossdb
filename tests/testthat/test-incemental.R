test_df <- data.frame(id = c("A1", "A1", "A1",
                             "A2", "A2", "A2",
                             "B1", "B1", 
                             "B2", "B2",
                             "B3", "B3",
                             "C1"),
                      origin = c(2012, 2012, 2012,
                                 2012, 2012, 2012,
                                 2013, 2013,
                                 2013, 2013,
                                 2013, 2013,
                                 2014),
                      dev = c(1, 2, 3,
                              1, 2, 3,
                              1, 2,
                              1, 2,
                              1, 2,
                              1),
                      paid_loss = c(10, 20, 25,
                                    0, 5, 5,
                                    20, 30,
                                    5, 6,
                                    1, 10,
                                    7)
)

test_df <- loss_df(test_df, id = "id", origin = "origin", dev = "dev", paid = "paid_loss")

test_df_incomplete <- test_df[-which(test_df[, "dev"] == 1 & test_df[, "origin"] == 2012), ]

