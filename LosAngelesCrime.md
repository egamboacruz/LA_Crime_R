# Introduction

This is an exploratory data analysis of all the different kinds of crime
committed in the city of Los Angeles within the years of **2020** &
**2021**. The point of this analysis to investigate the who, what, why,
when, and where of all the different crimes committed within two years
in the city of Los Angeles.

-   **The Who?:** Who are the victims (Race,Age,Sex)
-   **The What?:** What crime were they a victim of
-   **The When?:** When did this crime occur (Date and Time)
-   **The Where?:** Where do these crimes occur (Area and Premises
    Location)

This analysis will be an ongoing analysis with updates using data
directly from the Los Angeles Police Department.

# Data

This data is collected directly from the Los Angeles Public-Safety.

-   <https://data.lacity.org/Public-Safety/Crime-Data-from-2020-to-Present/2nrs-mtv8>

The code below shows the line of code use to collect the data.

    data<-read_csv("/Users/eduardogamboa/Documents/CSV_Files/LA_CrimeData(2020-2021).csv")

# Processing The Data

Combining columns date\_occ and time\_occ will make categorizing more
efficient, using functions like year(), month(), and hour() to find the
month year and hour.

    data <- data %>%
      unite("datetime_occ",
            sep = " ",
            date_occ:time_occ,na.rm = TRUE,
            remove = FALSE)

#### Replacing Victim Descent Code

The date uses letter codes for victims, I believe it makes it more
difficult to read since there are 16 different codes so I replaces them
with full words describing the victims descent.

-   The Code shows a single letter code (LEFT) being replace by the
    whole world (RIGHT).
-   The data Los Angeles public-safety website gives you the meaning of
    the codes.

<!-- -->

    data$vict_descent[data$vict_descent == "A"] <- "Asian"
    data$vict_descent[data$vict_descent == "B"] <- "Black"
    data$vict_descent[data$vict_descent == "C"] <- "Chineese"
    data$vict_descent[data$vict_descent == "D"] <- "Cambodian"
    data$vict_descent[data$vict_descent == "F"] <- "Filipino"
    data$vict_descent[data$vict_descent == "G"] <- "Guamaniam"
    data$vict_descent[data$vict_descent == "H"] <- "Hispanic/Latin/Mexican"
    # Removing mexican since they are Hispanic or Latin 
    data$vict_descent[data$vict_descent == "Hispanic/Latin/Mexican"] <- "Hispanic/Latin"
    #
    data$vict_descent[data$vict_descent == "I"] <- "American Indian"
    data$vict_descent[data$vict_descent == "J"] <- "Japanese"
    data$vict_descent[data$vict_descent == "K"] <- "Korean"
    data$vict_descent[data$vict_descent == "L"] <- "Laotian"
    data$vict_descent[data$vict_descent == "O"] <- "Other"
    data$vict_descent[data$vict_descent == "P"] <- "Pacific Islander"
    data$vict_descent[data$vict_descent == "S"] <- "Samoan"
    data$vict_descent[data$vict_descent == "U"] <- "Hawaiian"
    data$vict_descent[data$vict_descent == "V"] <- "Vietnamese"
    data$vict_descent[data$vict_descent == "W"] <- "White"
    data$vict_descent[data$vict_descent == "X"] <- "Unkown"
    data$vict_descent[data$vict_descent == "Z"] <- "Asian Indian"

I knew the next thing to process was going to be the victims age. I
wanted to check the

-   Youngest age of the victims.
-   Oldest age of the victims.

<!-- -->

    ## [1] -1

    ## [1] 120

This out-put told me that the youngest victim was -1 and I instantly
realized that was not right. The oldest age reported was 120 which is
surely possible so I wanted to take a closer look at these ages.

First I wanted to know how many cases contained a victim age less than
1.

    ## # A tibble: 2 × 2
    ##   vict_age total
    ##      <dbl> <int>
    ## 1       -1    17
    ## 2        0 98523

The data shows that there are a total of **17** cases with the age of
**-1** and **98523** cases with the victim age of **0**.

#### Deeper Look

    ## # A tibble: 138 × 3
    ## # Groups:   crm_cd_desc, vict_age [138]
    ##     crm_cd_desc                                                   vict_age     n
    ##     <chr>                                                            <dbl> <int>
    ##   1 ARSON                                                                0   549
    ##   2 ASSAULT WITH DEADLY WEAPON ON POLICE OFFICER                         0   476
    ##   3 ASSAULT WITH DEADLY WEAPON, AGGRAVATED ASSAULT                       0   903
    ##   4 ATTEMPTED ROBBERY                                                    0   269
    ##   5 BATTERY - SIMPLE ASSAULT                                             0   385
    ##   6 BATTERY ON A FIREFIGHTER                                             0    38
    ##   7 BATTERY POLICE (SIMPLE)                                              0   880
    ##   8 BATTERY WITH SEXUAL CONTACT                                          0    15
    ##   9 BEASTIALITY, CRIME AGAINST NATURE SEXUAL ASSLT WITH ANIM             0     2
    ##  10 BIGAMY                                                               0     1
    ##  11 BIKE - ATTEMPTED STOLEN                                              0     1
    ##  12 BIKE - STOLEN                                                        0    84
    ##  13 BOAT - STOLEN                                                        0    54
    ##  14 BOMB SCARE                                                           0   124
    ##  15 BRANDISH WEAPON                                                      0   154
    ##  16 BRIBERY                                                              0     2
    ##  17 BUNCO, ATTEMPT                                                       0    18
    ##  18 BUNCO, GRAND THEFT                                                   0   131
    ##  19 BUNCO, PETTY THEFT                                                   0    55
    ##  20 BURGLARY                                                             0  7547
    ##  21 BURGLARY FROM VEHICLE                                                0   731
    ##  22 BURGLARY FROM VEHICLE, ATTEMPTED                                     0    29
    ##  23 BURGLARY, ATTEMPTED                                                  0   484
    ##  24 CHILD ABANDONMENT                                                    0     5
    ##  25 CHILD ABUSE (PHYSICAL) - AGGRAVATED ASSAULT                          0    48
    ##  26 CHILD ABUSE (PHYSICAL) - SIMPLE ASSAULT                              0    47
    ##  27 CHILD ANNOYING (17YRS & UNDER)                                       0     4
    ##  28 CHILD NEGLECT (SEE 300 W.I.C.)                                       0   102
    ##  29 CHILD PORNOGRAPHY                                                    0     7
    ##  30 CHILD STEALING                                                       0    36
    ##  31 CONSPIRACY                                                           0     4
    ##  32 CONTEMPT OF COURT                                                    0    35
    ##  33 COUNTERFEIT                                                          0    31
    ##  34 CREDIT CARDS, FRAUD USE ($950 & UNDER                                0     5
    ##  35 CREDIT CARDS, FRAUD USE ($950.01 & OVER)                             0    15
    ##  36 CRIMINAL HOMICIDE                                                    0    14
    ##  37 CRIMINAL THREATS - NO WEAPON DISPLAYED                               0   265
    ##  38 CRM AGNST CHLD (13 OR UNDER) (14-15 & SUSP 10 YRS OLDER)             0    10
    ##  39 CRUELTY TO ANIMALS                                                   0    61
    ##  40 DEFRAUDING INNKEEPER/THEFT OF SERVICES, $950 & UNDER                 0    91
    ##  41 DEFRAUDING INNKEEPER/THEFT OF SERVICES, OVER $950.01                 0    19
    ##  42 DISCHARGE FIREARMS/SHOTS FIRED                                       0   970
    ##  43 DISHONEST EMPLOYEE - GRAND THEFT                                     0     5
    ##  44 DISHONEST EMPLOYEE - PETTY THEFT                                     0     4
    ##  45 DISRUPT SCHOOL                                                       0     1
    ##  46 DISTURBING THE PEACE                                                 0    43
    ##  47 DOCUMENT FORGERY / STOLEN FELONY                                     0   375
    ##  48 DOCUMENT WORTHLESS ($200 & UNDER)                                    0     9
    ##  49 DOCUMENT WORTHLESS ($200.01 & OVER)                                  0     9
    ##  50 DRIVING WITHOUT OWNER CONSENT (DWOC)                                 0    57
    ##  51 DRUGS, TO A MINOR                                                    0     5
    ##  52 EMBEZZLEMENT, GRAND THEFT ($950.01 & OVER)                           0  1439
    ##  53 EMBEZZLEMENT, PETTY THEFT ($950 & UNDER)                             0    24
    ##  54 EXTORTION                                                            0    22
    ##  55 FAILURE TO DISPERSE                                                  0     2
    ##  56 FAILURE TO YIELD                                                     0   388
    ##  57 FALSE IMPRISONMENT                                                   0     3
    ##  58 FALSE POLICE REPORT                                                  0    18
    ##  59 FIREARMS EMERGENCY PROTECTIVE ORDER (FIREARMS EPO)                   0     1
    ##  60 FIREARMS RESTRAINING ORDER (FIREARMS RO)                             0     1
    ##  61 GRAND THEFT / AUTO REPAIR                                            0     1
    ##  62 GRAND THEFT / INSURANCE FRAUD                                        0     1
    ##  63 HUMAN TRAFFICKING - COMMERCIAL SEX ACTS                              0    21
    ##  64 HUMAN TRAFFICKING - INVOLUNTARY SERVITUDE                            0     2
    ##  65 ILLEGAL DUMPING                                                      0    28
    ##  66 INDECENT EXPOSURE                                                    0    19
    ##  67 INTIMATE PARTNER - AGGRAVATED ASSAULT                                0    40
    ##  68 INTIMATE PARTNER - SIMPLE ASSAULT                                    0   216
    ##  69 KIDNAPPING                                                           0    52
    ##  70 KIDNAPPING - GRAND ATTEMPT                                           0     8
    ##  71 LETTERS, LEWD  -  TELEPHONE CALLS, LEWD                              0   113
    ##  72 LEWD CONDUCT                                                         0    14
    ##  73 LEWD/LASCIVIOUS ACTS WITH CHILD                                      0     2
    ##  74 LYNCHING                                                             0    10
    ##  75 LYNCHING - ATTEMPTED                                                 0     5
    ##  76 ORAL COPULATION                                                      0     3
    ##  77 OTHER ASSAULT                                                        0    42
    ##  78 OTHER MISCELLANEOUS CRIME                                            0  1263
    ##  79 PANDERING                                                            0    52
    ##  80 PEEPING TOM                                                          0     4
    ##  81 PETTY THEFT - AUTO REPAIR                                            0     4
    ##  82 PICKPOCKET                                                           0     1
    ##  83 PIMPING                                                              0     8
    ##  84 PROWLER                                                              0     4
    ##  85 PURSE SNATCHING                                                      0     2
    ##  86 RAPE, ATTEMPTED                                                      0     1
    ##  87 RAPE, FORCIBLE                                                       0    10
    ##  88 RECKLESS DRIVING                                                     0   104
    ##  89 REPLICA FIREARMS(SALE,DISPLAY,MANUFACTURE OR DISTRIBUTE)             0     2
    ##  90 RESISTING ARREST                                                     0   343
    ##  91 ROBBERY                                                              0  2538
    ##  92 SEX OFFENDER REGISTRANT OUT OF COMPLIANCE                            0   406
    ##  93 SEX,UNLAWFUL(INC MUTUAL CONSENT, PENETRATION W/ FRGN OBJ             0     3
    ##  94 SEXUAL PENETRATION W/FOREIGN OBJECT                                  0     6
    ##  95 SHOPLIFTING - ATTEMPT                                                0    27
    ##  96 SHOPLIFTING - PETTY THEFT ($950 & UNDER)                             0  3975
    ##  97 SHOPLIFTING-GRAND THEFT ($950.01 & OVER)                             0   625
    ##  98 SHOTS FIRED AT INHABITED DWELLING                                    0    61
    ##  99 SHOTS FIRED AT MOVING VEHICLE, TRAIN OR AIRCRAFT                     0    43
    ## 100 SODOMY/SEXUAL CONTACT B/W PENIS OF ONE PERS TO ANUS OTH              0     1
    ## 101 STALKING                                                             0     6
    ## 102 THEFT FROM MOTOR VEHICLE - ATTEMPT                                   0    33
    ## 103 THEFT FROM MOTOR VEHICLE - GRAND ($950.01 AND OVER)                  0   726
    ## 104 THEFT FROM MOTOR VEHICLE - PETTY ($950 & UNDER)                      0  8965
    ## 105 THEFT FROM PERSON - ATTEMPT                                          0     3
    ## 106 THEFT OF IDENTITY                                                    0   433
    ## 107 THEFT PLAIN - ATTEMPT                                                0   103
    ## 108 THEFT PLAIN - PETTY ($950 & UNDER)                                   0  2976
    ## 109 THEFT-GRAND ($950.01 & OVER) EXCPT, GUNS, FOWL, LIVESTK, PROD        0  2016
    ## 110 THEFT, COIN MACHINE - ATTEMPT                                        0     2
    ## 111 THEFT, COIN MACHINE - GRAND ($950.01 & OVER)                         0     2
    ## 112 THEFT, COIN MACHINE - PETTY ($950 & UNDER)                           0     6
    ## 113 THEFT, PERSON                                                        0    32
    ## 114 THREATENING PHONE CALLS/LETTERS                                      0    13
    ## 115 THROWING OBJECT AT MOVING VEHICLE                                    0    38
    ## 116 TILL TAP - GRAND THEFT ($950.01 & OVER)                              0     2
    ## 117 TILL TAP - PETTY ($950 & UNDER)                                      0     7
    ## 118 TRESPASSING                                                          0  1962
    ## 119 UNAUTHORIZED COMPUTER ACCESS                                         0    29
    ## 120 VANDALISM - FELONY ($400 & OVER, ALL CHURCH VANDALISMS)              0  6712
    ## 121 VANDALISM - MISDEAMEANOR ($399 OR UNDER)                             0  2705
    ## 122 VEHICLE - ATTEMPT STOLEN                                             0   120
    ## 123 VEHICLE - MOTORIZED SCOOTERS, BICYCLES, AND WHEELCHAIRS              0    12
    ## 124 VEHICLE - STOLEN                                                     0 44246
    ## 125 VIOLATION OF COURT ORDER                                             0    77
    ## 126 VIOLATION OF RESTRAINING ORDER                                       0   108
    ## 127 VIOLATION OF TEMPORARY RESTRAINING ORDER                             0     7
    ## 128 WEAPONS POSSESSION/BOMBING                                           0    15
    ## 129 BURGLARY                                                            -1     4
    ## 130 DOCUMENT FORGERY / STOLEN FELONY                                    -1     1
    ## 131 EMBEZZLEMENT, GRAND THEFT ($950.01 & OVER)                          -1     2
    ## 132 SEX OFFENDER REGISTRANT OUT OF COMPLIANCE                           -1     1
    ## 133 SHOPLIFTING - PETTY THEFT ($950 & UNDER)                            -1     1
    ## 134 THEFT FROM MOTOR VEHICLE - GRAND ($950.01 AND OVER)                 -1     1
    ## 135 THEFT OF IDENTITY                                                   -1     2
    ## 136 THEFT PLAIN - PETTY ($950 & UNDER)                                  -1     2
    ## 137 THEFT-GRAND ($950.01 & OVER) EXCPT, GUNS, FOWL, LIVESTK, PROD       -1     1
    ## 138 VANDALISM - FELONY ($400 & OVER, ALL CHURCH VANDALISMS)             -1     2

The table above shows all the crimes committed where the victim was
younger than 1 years old.While looking through the ages the only thing
that stood out to me was any observation that contained Child in it, or
animal. I believe the others means the age is unknown, the report was
most likely made anonymously by phone.
