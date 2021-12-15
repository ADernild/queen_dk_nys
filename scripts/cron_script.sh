#!/bin/bash
R < new_speech_appender.R --save
R < cleaning_speeches.R --save
R < preprocessing.R --save
R < sentiment_analysis.R --save
R < sentiment_sentences.R --save
R < stm.R --save
R < country_codes.R --save


