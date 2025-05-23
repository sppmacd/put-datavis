library("dplyr")
library("tidyr")
library("ggplot2")
library("stringr")
library("forcats")

# Note: setwd() to directory with csv files first

crewmate_techniques_usage = read.csv("crewmate_techniques_usage.csv")
impostor_techniques_usage = read.csv("impostor_techniques_usage.csv")
persuasion_techniques_usage = read.csv("persuasion_techniques_usage.csv")
persuasion_wins_analysis = read.csv("persuasion_wins_analysis.csv")
token_usage_640 = read.csv("token_usage_640.csv")
tournament_analysis_640 = read.csv("tournament_analysis_640.csv")

View(tournament_analysis_640)

model_mapping = list(
  'Claude 3.5 Haiku' = 'claude.3.5.haiku',
  'claude-3-5-haiku' =  'claude.3.5.haiku',
  'Claude 3.5 Sonnet' = 'claude.3.5.sonnet',
  'claude-3-5-sonnet' =  'claude.3.5.sonnet',
  'Gemini Flash 1.5' = 'gemini.flash.1.5',
  'gemini-flash-1-5' =  'gemini.flash.1.5',
  'Gemini Pro 1.5' = 'gemini.pro.1.5',
  'gemini-pro-1-5' =  'gemini.pro.1.5',
  'GPT-4o mini' = 'gpt.4o.mini',
  'gpt-4o-mini' =  'gpt.4o.mini',
  'GPT-4o' = 'gpt.4o',
  'gpt-4o' =  'gpt.4o',
  'Llama 3.1 405B' = 'llama.3.1.405b.instruct',
  'llama-3-1-405b-instruct' =  'llama.3.1.405b.instruct',
  'Llama 3.1 8B' = 'llama.3.1.8b.instruct',
  'llama-3-1-8b-instruct' =  'llama.3.1.8b.instruct'
)

models = tournament_analysis_640 %>%
       select(Impostor,ImpostorSize) %>%
       unique() %>%
       transmute(name=unlist(model_mapping[Impostor]), size=ImpostorSize)
models
# persuasion_techniques_usage is just sum of
#   crewmate_techniques_usage + impostor_techniques_usage

View(token_usage_640)

token_usage_640 %>%
  group_by(imp_model, crew_model) %>%
  summarise(imp_tok_total=sum(imp_out_tokens), crew_tok_total=sum(crew_out_tokens)) %>%
  mutate(vs=paste(model_mapping[imp_model], "(vs", model_mapping[crew_model], ")")) -> tu2
tu2

tournament_analysis_640
# this one is nice for comparison of specific models
ggplot(tournament_analysis_640, aes(x=Crewmate, y=Impostor, fill=Wins/(Wins+Losses))) +
  geom_raster() +
  scale_fill_gradient2(low="blue", high="red", mid="white", midpoint=0.5)

# another idea - nice for finding who is winning
ta2 = tournament_analysis_640 %>%
  mutate(imp_model=Impostor, vs=paste(model_mapping[Impostor], "(vs", model_mapping[Crewmate], ")"), wr=Wins/(Wins+Losses)) %>%
  inner_join(tu2, by="vs")
ta2 = ta2 %>% arrange(-wr) %>% filter(row_number()<=5)
#ta2 %>% gather(imp_or_crew_tokens, tokens, c(imp_tok_total, crew_tok_total)) -> ta2
ta2

# fill is the cost (total used tokens by Impostor in this configuration)
ggplot(ta2, aes(x=wr, y=reorder(vs, wr), fill=imp_tok_total+crew_tok_total)) +
  scale_fill_gradient(low="green", high="red") +
  geom_bar(stat = "identity", position = position_dodge(0.8))

########

# the most expensive models
token_usage_640 %>%
  group_by(imp_model, crew_model) %>%
  summarise(rounds=sum(rounds), imp_tok_total=sum(imp_out_tokens), crew_tok_total=sum(crew_out_tokens)) -> tu_sum
tu_sum

ggplot(tu_sum, aes(x=imp_model, y=imp_tok_total, fill=rounds)) +
  geom_bar(stat = "identity")

########

cw_tidy = (crewmate_techniques_usage %>% pivot_longer(2:9) %>%
          select(-Total.Techniques) %>% full_join(models, by="name"))

model_pcount = cw_tidy %>% group_by(size) %>% summarise(s = sum(value))
cw_tidy_norm = cw_tidy %>% inner_join(model_pcount, by = "size") %>% mutate(value_norm=value/s*100)
cw_tidy_norm

# % of techniques used by a given model size
ggplot(cw_tidy_norm, aes(x=value_norm, y=reorder(Persuasion.Technique, value_norm), fill=size)) +
  geom_bar(position=position_dodge(width = 0.8), stat="identity")

# total techniques, grouped by model
# I'd like also to have these "stack fills" ordered by factor
# i.e the model that uses most of a given factor will be at the left.
ggplot(cw_tidy, aes(fill=name, x=Persuasion.Technique, y=value)) +
  geom_bar(stat="identity") +
  facet_grid(~size) +
  coord_flip()

# Better one for comparison of techniques between models
#cw_tidy_norm = cw_tidy_norm %>% arrange(-value_norm) %>% filter(row_number()<=5*8)
ggplot(cw_tidy_norm, aes(fill=reorder(name, size), x=reorder(Persuasion.Technique, value_norm), y=value_norm)) +
  geom_bar(position=position_dodge(width = 0.7), stat="identity") +
  labs(x="Technique", y="% of technique uses in model size") +
  coord_flip()

########

tu2

ggplot(tu2, aes(x=imp_model, y=crew_model, size=imp_tok_total))
