test_that("Output from data_summary function is
          dataframe with correct content for all
          positive control data from the
          validation study", {
            jap_uns <-
              validity(xeredar::valid_data_xeta[["triac_japan_combined"]],
                       xeredar::valid_data_xeta[["triac_japan_unspiked"]],
                       row = FALSE, dagostino = FALSE, alpha = 0.01)
            jap_unssum <- data_summary(jap_uns)
            expect_equal(jap_unssum$`0.01`, c(
              "-4.95", "12.04",
              "14.77", "6.29",
              "ns", "ns", "ns"
            ))
            expect_equal(jap_unssum$`0.1`, c(
              "-1.54", "18.66",
              "20.58", "11.53",
              "ns", "*", "ns"
            ))
            expect_equal(jap_unssum$`1`, c(
              "22.86", "47.53",
              "64.98", "42.58",
              "*", "*", "ns"
            ))
            expect_equal(jap_unssum$`2`, c(
              "41.35", "58.38",
              "87.22", "58.3",
              "*", "*", "ns"
            ))
            expect_equal(jap_unssum$`5`, c(
              "66.6", "95.38",
              "126.76", "92.23",
              "*", "*", "ns"
            ))
            jap_sp <-
              validity(xeredar::valid_data_xeta[["triac_japan_combined"]],
                       xeredar::valid_data_xeta[["triac_japan_spiked"]],
                       row = FALSE, dagostino = FALSE, alpha = 0.005)
            jap_spsum <- data_summary(jap_sp)
            expect_equal(jap_spsum$`0.01`, c(
              "-8.57", "7.36",
              "-14.11", "-3.75",
              "ns", "ns", "ns"
            ))
            expect_equal(jap_spsum$`0.1`, c(
              "9.24", "15.43",
              "9.8", "11.39",
              "ns", "*", "ns"
            ))
            expect_equal(jap_spsum$`1`, c(
              "14.22", "24.27",
              "30.01", "22.23",
              "*", "*", "ns"
            ))
            expect_equal(jap_spsum$`2`, c(
              "14.45", "28.79",
              "42.56", "27.3",
              "*", "*", "ns"
            ))
            expect_equal(jap_spsum$`5`, c(
              "12.08", "24.51",
              "39.37", "24.39",
              "*", "*", "ns"
            ))
            fran_uns <-
              validity(xeredar::valid_data_xeta[["triac_france_combined"]],
                       xeredar::valid_data_xeta[["triac_france_unspiked"]],
                       row = FALSE, dagostino = FALSE)
            fran_unssum <- data_summary(fran_uns)
            expect_equal(fran_unssum$`0.01`, c(
              "12.87", "6.71",
              "5.25", "7.85",
              "ns", "ns", "ns"
            ))
            expect_equal(fran_unssum$`0.1`, c(
              "6.86", "2.54",
              "11.5", "7.08",
              "ns", "ns", "ns"
            ))
            expect_equal(fran_unssum$`1`, c(
              "79.57", "62.75",
              "86.83", "77.39",
              "*", "*", "ns"
            ))
            expect_equal(fran_unssum$`2`, c(
              "103.49", "106.32",
              "94.64", "101.47",
              "*", "*", "ns"
            ))
            expect_equal(fran_unssum$`5`, c(
              "137.71", "122.46",
              "120.81", "126.65",
              "*", "*", "ns"
            ))
            fran_sp <-
              validity(xeredar::valid_data_xeta[["triac_france_combined"]],
                       xeredar::valid_data_xeta[["triac_france_spiked"]],
                       row = FALSE, dagostino = FALSE, alpha = 0.005)
            fran_spsum <- data_summary(fran_sp)
            expect_equal(fran_spsum$`0.01`, c(
              "-2.24", "-20.49",
              "-10.74", "-11.34",
              "ns", "ns", "ns"
            ))
            expect_equal(fran_spsum$`0.1`, c(
              "4.52", "-16.64",
              "0.22", "-3.81",
              "ns", "ns", "ns"
            ))
            expect_equal(fran_spsum$`1`, c(
              "19.09", "14.65",
              "11.63", "14.76",
              ".", "*", "ns"
            ))
            expect_equal(fran_spsum$`2`, c(
              "28.53", "25.53",
              "30.73", "28.51",
              "*", "*", "ns"
            ))
            expect_equal(fran_spsum$`5`, c(
              "12.19", "15.68",
              "19.77", "16.36",
              "*", "*", "ns"
            ))
            usa_uns <-
              validity(xeredar::valid_data_xeta[["triac_usa_combined"]],
                       xeredar::valid_data_xeta[["triac_usa_unspiked"]],
                       row = FALSE, dagostino = FALSE, alpha = 0)
            usa_unssum <- data_summary(usa_uns)
            expect_equal(usa_unssum$`0.01`, c(
              "17.48", "16.15",
              "28.54", "21.34",
              "ns", "*", "ns"
            ))
            expect_equal(usa_unssum$`0.1`, c(
              "31.01", "33.62",
              "56.24", "39.96",
              "*", "*", "ns"
            ))
            expect_equal(usa_unssum$`1`, c(
              "68.75", "91.42",
              "106.57", "88.23",
              "*", "*", "ns"
            ))
            expect_equal(usa_unssum$`2`, c(
              "105.09", "111.04",
              "96.6", "104.82",
              "*", "*", "ns"
            ))
            expect_equal(usa_unssum$`5`, c(
              "94.27", "117.41",
              "102.99", "102.75",
              "*", "*", "ns"
            ))
            usa_sp <- validity(xeredar::valid_data_xeta[["triac_usa_combined"]],
                               xeredar::valid_data_xeta[["triac_usa_spiked"]],
                               row = FALSE, dagostino = FALSE)
            usa_spsum <- data_summary(usa_sp)
            expect_equal(usa_spsum$`0.01`, c(
              "5.87", "39.74",
              "2.44", "10.38",
              "ns", "ns", "ns"
            ))
            expect_equal(usa_spsum$`0.1`, c(
              "11.81", "60.44",
              "17.37", "21.08",
              "*", "*", "ns"
            ))
            expect_equal(usa_spsum$`1`, c(
              "27.71", "52.5",
              "21.19", "29.9",
              "*", "*", "ns"
            ))
            expect_equal(usa_spsum$`2`, c(
              "33.39", "92.78",
              "19.99", "39.07",
              "*", "*", "ns"
            ))
            expect_equal(usa_spsum$`5`, c(
              "35.46", "59.27",
              "28.82", "36.42",
              "*", "*", "ns"
            ))
          })
