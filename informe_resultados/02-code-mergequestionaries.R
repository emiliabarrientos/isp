####### Unir cuestionarios #############
# Nicolas Ratto
library(dplyr)
library(haven)
library(tidyverse)
library(readxl)
library(tidyverse)

#### Combinar variables ####

# q0056

ISP<-ISP %>% mutate(q56_65=case_when(q0055==2 ~ q0056,
                                q0055==1 ~ q0065,
                                TRUE ~ NA_character_))

# q0057

ISP %>% subset(!is.na(q0057_0001)&!is.na(ISP$q0066_0001)) %>% select(respondent_id,q0057_0001,q0066_0001,q0055,q57_1)

ISP<-ISP %>% mutate(q57_1=case_when(q0055==2 ~ q0057_0001, q0055==1 ~ q0066_0001, TRUE ~ NA_real_),
                    q57_2=case_when(q0055==2 ~ q0057_0002, q0055==1 ~ q0066_0002, TRUE ~ NA_real_),
                    q57_3=case_when(q0055==2 ~ q0057_0003, q0055==1 ~ q0066_0003, TRUE ~ NA_real_),
                    q57_4=case_when(q0055==2 ~ q0057_0004, q0055==1 ~ q0066_0004, TRUE ~ NA_real_))

# q0058

ISP<-ISP %>% mutate(q58_1=case_when(q0055==2 ~ q0058_0001, q0055==1 ~ q0067_0001, TRUE ~ NA_real_),
                    q58_2=case_when(q0055==2 ~ q0058_0002, q0055==1 ~ q0067_0002, TRUE ~ NA_real_),
                    q58_3=case_when(q0055==2 ~ q0058_0003, q0055==1 ~ q0067_0003, TRUE ~ NA_real_))


# q0059

ISP<-ISP %>% mutate(q59_1=case_when(q0055==2 ~ q0059_0001, q0055==1 ~ q0068_0001, TRUE ~ NA_real_),
                    q59_2=case_when(q0055==2 ~ q0059_0002, q0055==1 ~ q0068_0002, TRUE ~ NA_real_),
                    q59_3=case_when(q0055==2 ~ q0059_0003, q0055==1 ~ q0068_0003, TRUE ~ NA_real_),
                    q59_4=case_when(q0055==2 ~ q0059_0004, q0055==1 ~ q0068_0004, TRUE ~ NA_real_),
                    q59_5=case_when(q0055==2 ~ q0059_0005, q0055==1 ~ q0068_0005, TRUE ~ NA_real_))

# q0060

ISP<-ISP %>% mutate(q60_1=case_when(q0055==2 ~ q0060_0001, q0055==1 ~ q0069_0001, TRUE ~ NA_real_),
                    q60_2=case_when(q0055==2 ~ q0060_0002, q0055==1 ~ q0069_0002, TRUE ~ NA_real_),
                    q60_3=case_when(q0055==2 ~ q0060_0003, q0055==1 ~ q0069_0003, TRUE ~ NA_real_))


# q0061

ISP<-ISP %>% mutate(q61_1=case_when(q0055==2 ~ q0061_0001, q0055==1 ~ q0070_0001, TRUE ~ NA_real_),
                    q61_2=case_when(q0055==2 ~ q0061_0002, q0055==1 ~ q0070_0002, TRUE ~ NA_real_),
                    q61_3=case_when(q0055==2 ~ q0061_0003, q0055==1 ~ q0070_0003, TRUE ~ NA_real_),
                    q61_4=case_when(q0055==2 ~ q0061_0004, q0055==1 ~ q0070_0004, TRUE ~ NA_real_),
                    q61_5=case_when(q0055==2 ~ q0061_0005, q0055==1 ~ q0070_0005, TRUE ~ NA_real_),
                    q61_6=case_when(q0055==2 ~ q0061_0006, q0055==1 ~ q0070_0006, TRUE ~ NA_real_),
                    q61_7=case_when(q0055==2 ~ q0061_0007, q0055==1 ~ q0070_0007, TRUE ~ NA_real_),
                    q61_8=case_when(q0055==2 ~ q0061_0008, q0055==1 ~ q0070_0008, TRUE ~ NA_real_))

# q0062

ISP<-ISP %>% mutate(q62_71=case_when(q0055==2 ~ q0062,
                                     q0055==1 ~ q0071,
                                     TRUE ~ NA_character_))
table(ISP$q0062)
table(ISP$q0071)

# q0063

ISP$q0072_0001<-as.numeric(as.factor(as.numeric(ISP$q0072_0001)))
ISP$q0072_0002<-as.numeric(as.factor(as.numeric(ISP$q0072_0002)))

ISP<-ISP %>% mutate(q63_1=case_when(q0055==2 ~ q0063_0001, q0055==1 ~ q0072_0001, TRUE ~ NA_real_),
                    q63_2=case_when(q0055==2 ~ q0063_0002, q0055==1 ~ q0072_0002, TRUE ~ NA_real_))

table(ISP$q63_1)

table(ISP$q0063_0001)
table(ISP$q0072_0001)

# q0064
ISP$q0064<-as.character(ISP$q0064)
ISP$q0073<-as.character(ISP$q0073)

ISP<-ISP %>% mutate(q64_73=case_when(q0055==2 ~ q0064,
                                     q0055==1 ~ q0073,
                                     TRUE ~ NA_character_))
## Variables finales son 
##q56_65 ##q57_1 ##q58_1 ##q59_1 ##q60_1 ##61_1 ##62_71 ##q63_1 ##q64_73