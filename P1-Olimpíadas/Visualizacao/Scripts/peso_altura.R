## packages
pacman::p_load(tidyverse, # manipulação de variáveis e ggplot2
               cowplot,   # unir os plots com o ggdraw 
               ggtext,    # renderização de texto
               ggview)    # auxilio na visualização do plot





## fontes de texto
sysfonts::font_add_google("Quicksand","quicksand") # fonte padrao
showtext::showtext_auto()                          # renderização de texto
showtext::showtext_opts(dpi=150)                   # renderização de imagem





## datasets
nao_medalhistas = read.csv('https://raw.githubusercontent.com/taferreiraua/LAB-CD/main/P1-Olimp%C3%ADadas/Datasets/nao_medalhistas.csv')
medalhistas = read.csv('https://raw.githubusercontent.com/taferreiraua/LAB-CD/main/P1-Olimp%C3%ADadas/Datasets/medalhistas.csv')






## manipulações relacionadas ao gráfico
nao_medalhistas = nao_medalhistas |>
  mutate(Esporte = case_when(Esporte=='Basketball'~'Basquete',
                             Esporte=='Football'~'Futebol',
                             Esporte=='Volleyball'~'Voleibol',
                             Esporte=='Beach Volleyball' ~ 'Voleibol<br>de praia',
                             Esporte=='Judo' ~ 'Judô')) |>
  group_by(Esporte) |>
  mutate(cor = round(cor(Altura, Peso), 4),
         cor_type = case_when(cor < 0 ~ 'negativa',
                              cor > 0 ~ 'positiva',
                              TRUE ~ 'Indeterminada'),
         cor_grau = case_when(abs(cor) > 0 & abs(cor) <0.3 ~ 'fraca',
                              abs(cor) >= 0.3 & abs(cor) <0.6 ~ 'regular',
                              abs(cor) >= 0.6 & abs(cor) <0.9 ~ 'forte',
                              abs(cor) >= 0.9 ~ 'muito forte',
                              TRUE ~ 'nula')) |>
  ungroup() |>
  mutate(strip.title = factor(Esporte, levels=Esporte, 
                              labels=paste0("<span style='font-size:100pt;color:#BC4B51;'>", toupper(Esporte), '<br></span>'))) |>
  arrange(strip.title)

  
medalhistas = medalhistas |>
  mutate(Esporte = case_when(Esporte=='Basketball'~'Basquete',
                             Esporte=='Football'~'Futebol',
                             Esporte=='Volleyball'~'Voleibol',
                             Esporte=='Beach Volleyball' ~ 'Voleibol<br>de praia',
                             Esporte=='Judo' ~ 'Judô')) |>
  group_by(Esporte) |>
  mutate(cor = round(cor(Altura, Peso), 4),
         cor_type = case_when(cor < 0 ~ 'negativa',
                              cor > 0 ~ 'positiva',
                              TRUE ~ 'Indeterminada'),
         cor_grau = case_when(abs(cor) > 0 & abs(cor) <0.3 ~ 'fraca',
                              abs(cor) >= 0.3 & abs(cor) <0.6 ~ 'regular',
                              abs(cor) >= 0.6 & abs(cor) <0.9 ~ 'forte',
                              abs(cor) >= 0.9 ~ 'muito forte',
                              TRUE ~ 'nula')) |>
  ungroup() |>
  mutate(strip.title = factor(Esporte, levels=Esporte, 
                              labels=paste0("<span style='font-size:100pt;color:#BC4B51;'>", toupper(Esporte), '<br></span>'))) |>
  arrange(strip.title)






## plot 1 - Todas as competidoras
ggplot(nao_medalhistas, aes(x=Altura, y=Peso)) +
  geom_point(size=4, alpha=.5) +
  geom_smooth(method = 'lm', color='gold', size=2.4) +
  facet_wrap(.~factor(strip.title, stringr::str_sort(unique(strip.title))), ncol=5) +
  geom_richtext(aes(x=160, y=130, label=paste0("<span style='font-size:45pt;'>", 'COR ', 
                                               "<span style='font-size:70pt;'>", cor, "<br>", 
                                               "<span style='font-size:45pt;'>", toupper(cor_type), ' & ', toupper(cor_grau))),
                family='quicksand',
                color='#444455',
                hjust=0,
                vjust=1,
                label.color=NA,
                fill=NA
                ) +
  scale_y_continuous(limits=c(30, 130), breaks=seq(35, 105, 10)) +
  scale_x_continuous(breaks=seq(160, 200, 10)) +
  labs(title=paste0("<span style='font-size:140pt;color:#444455;'>", 'CORRELAÇÕES: ', 
                    "<span style='font-size:90pt;color:#BC4B51;'><b>", 'Altura vs. Peso'),
       y='<i>NAO MEDALHISTAS') +
  theme_minimal() +
  theme(plot.background = element_rect(fill='#F5F3F4', color='#F5F3F4'),
        plot.title = element_markdown(family='quicksand',
                                      hjust=0,
                                      margin=margin(t=40)),
        plot.margin = margin(r=20, b=15, l=25),
        panel.grid = element_blank(),
        strip.text = element_markdown(family='quicksand',
                                      vjust=0,
                                      hjust=.5),
        axis.title.y = element_markdown(color='#444455',
                                        size=70,
                                        margin=margin(r=30)),
        axis.title.x = element_blank(),
        axis.text.x = element_markdown(size=33, angle=90),
        axis.text.y = element_markdown(size=33))




## salvando p1
ggview(units='px', height=2500, width=8000)
ggsave(units='px', height=2500, width=8000, 
       filename='C:/Users/Thays Ferreira/Documents/LAB/Script/Correlacoes/peso_altura/p1.png')








## plot 2 - Somente medalhistas
ggplot(medalhistas, aes(x=Altura, y=Peso)) +
  geom_point(size=4, alpha=.5) +
  geom_smooth(method = 'lm', color='gold', size=2.4) +
  facet_wrap(.~factor(strip.title, stringr::str_sort(unique(strip.title))), ncol=5) +
  geom_richtext(aes(x=154, y=110, label=paste0("<span style='font-size:45pt;'>", 'COR ', 
                                               "<span style='font-size:70pt;'>", cor, "<br>", 
                                               "<span style='font-size:45pt;'>", toupper(cor_type), ' & ', toupper(cor_grau))),
                family='quicksand',
                color='#444455',
                hjust=0,
                vjust=1,
                label.color=NA,
                fill=NA
  ) +
  scale_y_continuous(limits=c(40, 110), breaks=seq(48, 105, 10)) +
  scale_x_continuous(breaks=seq(160, 200, 10)) +
  labs(y='<i>MEDALHISTAS') +
  theme_minimal() +
  theme(plot.background = element_rect(fill='#F5F3F4', color='#F5F3F4'),
        plot.margin = margin(r=20, b=40, l=25),
        panel.grid = element_blank(),
        strip.text = element_blank(),
        axis.title.y = element_markdown(color='#444455',
                                        size=70,
                                        margin=margin(r=30)),
        axis.title.x = element_blank(),
        axis.text.x = element_markdown(size=33, angle=90),
        axis.text.y = element_markdown(size=33))



## salvando p2
ggview(units='px', height=1700, width=8000)
ggsave(units='px', height=1700, width=8000, 
       filename='C:/Users/Thays Ferreira/Documents/LAB/Script/Correlacoes/peso_altura/p2.png')




## Unindo as duas partes usando ggdraw

## criando um 'pano de fundo'
plot = ggplot() +
  aes(x=seq(-3, 3), y=seq(-3, 3)) +
  theme_void() +
  theme(plot.background = element_rect(fill='#F5F3F4', color='#F5F3F4'))





## juntando os dois graficos
ggdraw(plot) +
  draw_image(magick::image_read('C:/Users/Thays Ferreira/Documents/LAB/Script/Correlacoes/peso_altura/p1.png'),
             x=0, y=.207, scale=1) +
  draw_image(magick::image_read('C:/Users/Thays Ferreira/Documents/LAB/Script/Correlacoes/peso_altura/p2.png'),
             x=0, y=-.265, scale=1)





## salvando
ggview(units='px', height=4000, width=7200)
ggsave(units='px', height=4000, width=7200, filename='peso_altura.png')
