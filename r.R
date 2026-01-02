


# TESTE T

Nossa base de dados apresenta 3 espécies de pengui diferentes variáveis associadas. a ela. Para iniciarmos,
temos como objetivo é pensar analise estatísica basicas. A ideia incial é observar se existe diferença entre
as espécies de pinguins segundo alguns critérior

pinguim$Especies

b <-pinguim %>% filter(Especies %in% c("Adelie", "Gentoo"))



pinguim$sex

adelie_m <-pinguim %>% 
  filter(species == "Adelie",
         sex == "male") %>% 
  mutate(peso_adelie_masc = body_mass_g) %>% 
  select(peso_adelie_masc, species)



adelie_f <-pinguim %>% 
  filter(species == "Adelie",
         sex == "female") %>% 
  mutate(padelie_fe = body_mass_g) %>% 
  select(peso_adelie_fe)

adelie_peso<- tibble(adelie_f, adelie_m) %>%
  pivot_longer(!species, names_to = "Sexo", values_to = "massa_g" ) %>% 
  mutate(Sexo = as.factor(Sexo),
         massa_g = as.numeric(massa_g) )


ggplot(adelie_peso, aes(y = massa_g, x = Sexo,  fill = Sexo))+
  geom_boxplot()+
  theme_classic()+
  theme(legend.position = "bottom")

ggplot(adelie_peso)




teste_t <- t.test(adelie_m$peso_adelie_masc, adelie_f$peso_adelie_fe)
broom::tidy(teste_t)



# ANOVA

Quando buscamos analisar 2 ou mais grupos, ou tratamentos, utilizamos outras ferramentas
estatísticas, Anova.

A análise de variávencia dos dados que utiliza-se da média dos grupos buscando verificar 
se existe diferença entre eles. Assim como o test t, utilizaremos dos princípios do h1 e h0

Nosso h0 demontra não existe diferença entre os grupos
Nosso h1 significa que tem diferença entre os grupos.

Assim, utilizando valor de 0.95 de confiabilidade, o valor de p < 0.05 irá ser refutada.


Objetivo do nosso script será

Existe diferença estatística entre especies de penguin a partir do peso? Lembrando que nossa base de
dados apresentam 3 especíes, Gentoo, Adelia e Chinstrap

Dessa forma nossa hipótese são:
  
Ho - O peso não promve diferença entre os três tipos de espécies
H1 - O peso promve diferença entre os três tipos de espécies

Importante lembrar que o peso é uma variável dependete
pacotes

pwr

penguins_anova<- penguins %>% 
  select(species, body_mass_g) %>% 
  na.omit()



aov_penguins<-aov(body_mass ~ species, data = p)

anova()


pwr::pwr.anova.test(aov_penguins)
effectsize::eta_squared(model = aov_penguins)

penguins$species

library(ggstatsplot)



PlantGrowth$group <- factor(PlantGrowth$group,
                            labels = c("Controle",
                                       "Tratamento",
                                       "Tratamento 2"))



PlantGrowth_aov<- aov(weight ~ group,
                      data = PlantGrowth)

library(ggstatsplot)

p <- penguins %>% na.omit()
names(p)

ggstatsplot::ggbetweenstats(x =  species, 
                            y = body_mass,
                            data = p,
                            type = "parametric")


shapiro.test(residuals(aov_penguins))
qqnorm(residuals(aov_penguins))
qqline(residuals(aov_penguins))

car::leveneTest(body_mass ~ species, data = p)

summary(aov_penguins)


TukeyHSD()
anova()


anova()



adelie$body_mass_g
t.test()


p %>% dplyr::select(species, body_mass)
adelie
a$Especies
