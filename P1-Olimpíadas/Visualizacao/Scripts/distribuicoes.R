## packages
pacman::p_load(tidyverse, # manipulação de variáveis e ggplot2
               cowplot,   # ggdraw (unir plots)
               ggtext,    # renderização de texto
               ggridges,  # para a densidade
               ggimage,   # adicionar imagens ao plot
               ggview)    # auxilio na visualização do plot







## fontes de texto
sysfonts::font_add_google("Quicksand","quicksand") # fonte padrao
showtext::showtext_auto()                          # renderização de texto
showtext::showtext_opts(dpi=150)                   # renderização de imagem





## datasets
nao_medalhistas = read.csv('https://raw.githubusercontent.com/taferreiraua/LAB-CD/main/P1-Olimp%C3%ADadas/Datasets/nao_medalhistas.csv')
medalhistas = read.csv('https://raw.githubusercontent.com/taferreiraua/LAB-CD/main/P1-Olimp%C3%ADadas/Datasets/medalhistas.csv')


nao_medalhistas = nao_medalhistas |>
  mutate(Esporte = case_when(Esporte=='Basketball'~'Basquete',
                             Esporte=='Football'~'Futebol',
                             Esporte=='Volleyball'~'Voleibol',
                             Esporte=='Beach Volleyball' ~ 'Voleibol<br>de praia',
                             Esporte=='Judo' ~ 'Judô'),
         Esporte = toupper(Esporte))


medalhistas = medalhistas |>
  mutate(Esporte = case_when(Esporte=='Basketball'~'Basquete',
                             Esporte=='Football'~'Futebol',
                             Esporte=='Volleyball'~'Voleibol',
                             Esporte=='Beach Volleyball' ~ 'Voleibol<br>de praia',
                             Esporte=='Judo' ~ 'Judô'),
         Esporte = toupper(Esporte))






## plot

## Idade + titulo
ggplot() +
  geom_density(data = nao_medalhistas,
               aes(x=Idade, group=Esporte),
               fill='#BC4B51', 
               alpha=1,
               linewidth=0) +
  geom_density(data = medalhistas,
               aes(x=Idade), 
               fill='gold', 
               alpha=.5, 
               linewidth=0) +
  scale_x_continuous(limits=c(10, 50), breaks=seq(15, 40, 5)) +
  facet_wrap(~Esporte, ncol=5) +
  labs(title=paste0('Comparação da distribuição de <b>idade, peso</b> e <b>altura</b> entre atletas<br>',
                    "<span style='color:#FCA311;'><b>medalhistas</span> e",
                    "<span style='color:#BC4B51;'><b> não medalhistas</span>."),
       y='Idade') +
  theme_minimal() +
  theme(plot.background = element_rect(fill='#F5F3F4', color='#F5F3F4'),
        plot.title = element_markdown(size=100,
                                      hjust=0,
                                      lineheight=.5,
                                      family='quicksand',
                                      color='#444455'),
        strip.text = element_markdown(size=90,
                                      lineheight=.5,
                                      vjust=0,
                                      color='#444455',
                                      family='quicksand',
                                      face='bold'),
        panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_markdown(size=100,
                                        family='quicksand',
                                        color='#444455'),
        axis.text.x = element_markdown(size=45,
                                       family='quicksand'),
        axis.text.y = element_blank(),
        legend.position = 'none')


## salvando
ggview(units='px', height=2000, width=7000)
ggsave(units='px', height=2000, width=7000, filename='p1.png')





## peso
ggplot() +
  geom_density(data = nao_medalhistas,
               aes(x=Peso, group=Esporte),
               fill='#BC4B51', 
               alpha=1,
               linewidth=0) +
  geom_density(data = medalhistas,
               aes(x=Peso), 
               fill='gold', 
               alpha=.5, 
               linewidth=0) +
  scale_x_continuous(limits=c(25, 115), breaks=seq(30, 110, 15)) +
  facet_wrap(~Esporte, ncol=5) +
  labs(y='Peso') +
  theme_minimal() +
  theme(plot.background = element_rect(fill='#F5F3F4', color='#F5F3F4'),
        strip.text = element_blank(),
        panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_markdown(size=100,
                                        family='quicksand',
                                        color='#444455'),
        axis.text.x = element_markdown(size=45,
                                       family='quicksand'),
        axis.text.y = element_blank(),
        legend.position = 'none')


## salvando
ggview(units='px', height=1500, width=7000)
ggsave(units='px', height=1500, width=7000, filename='p2.png')








## altura
ggplot() +
  geom_density(data = nao_medalhistas,
               aes(x=Altura, group=Esporte),
               fill='#BC4B51', 
               alpha=1,
               linewidth=0) +
  geom_density(data = medalhistas,
               aes(x=Altura), 
               fill='gold', 
               alpha=.5, 
               linewidth=0) +
  scale_x_continuous(limits=c(140, 210), breaks=seq(150, 200, 10)) +
  facet_wrap(~Esporte, ncol=5) +
  labs(y='Altura') +
  theme_minimal() +
  theme(plot.background = element_rect(fill='#F5F3F4', color='#F5F3F4'),
        strip.text = element_blank(),
        panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_markdown(size=100,
                                        family='quicksand',
                                        color='#444455'),
        axis.text.x = element_markdown(size=45,
                                       family='quicksand'),
        axis.text.y = element_blank(),
        legend.position = 'none')


## salvando
ggview(units='px', height=1500, width=7000)
ggsave(units='px', height=1500, width=7000, filename='p3.png')









## Unindo as duas partes usando ggdraw

## criando um 'pano de fundo'
plot = ggplot() +
  aes(x=seq(-3, 3), y=seq(-3, 3)) +
  theme_void() +
  theme(plot.background = element_rect(fill='#F5F3F4', color='#F5F3F4'))








## juntando os graficos
ggdraw(plot) +
  draw_image(magick::image_read('~/p1.png'),
             x=0, y=.29, scale=.94) +
  draw_image(magick::image_read('~/p2.png'),
             x=0, y=-0.025, scale=.94) +
  draw_image(magick::image_read('~/p3.png'),
             x=0, y=-.32, scale=.94)





## salvando plot final
ggview(units='px', height=4800, width=6000)
ggsave(units='px', height=4800, width=6000, filename='distribuicoes.png')
