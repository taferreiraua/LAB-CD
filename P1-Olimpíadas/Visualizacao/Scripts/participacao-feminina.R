## packages
pacman::p_load(tidyverse, # manipulação de variáveis e ggplot2
               ggtext,    # renderização de texto
               ggimage,   # adicionar imagens ao plot
               ggview)    # auxilio na visualização do plot





## dados
participantes = read.csv('https://raw.githubusercontent.com/taferreiraua/LAB-CD/main/P1-Olimp%C3%ADadas/Datasets/participantes.csv')




## fontes de texto
sysfonts::font_add_google("Quicksand","quicksand") # fonte padrao
showtext::showtext_auto()                          # renderização de texto
showtext::showtext_opts(dpi=150)                   # renderização de imagem





## pegando as imagens do github
links = paste0('https://raw.githubusercontent.com/taferreiraua/LAB-CD/main/P1-Olimp%C3%ADadas/Visualizacao/Imagens/', 
               c('maria', 'jaque', 'sandra'), '.png')





## mapeando itens do plot
map_img = data.frame(
  img = links,
  x = c(1932, 1990, 2002),
  y = c(265, 290, 290)
)

map_text = data.frame(
  label = c('<b>Primeira<br>participação<br>de mulheres<br>no evento',
            'Aos 17 anos, a nadadora<br><b>Maria Lenk</b> se torna a<br>primeira mulher brasileira<br>a competir nas olimpíadas',
            'A equipe de vôlei de praia<br><b>Jacqueline Silva e Sandra Pires</b><br>conquista o primeiro ouro<br>feminino brasileiro',
            paste0('Pela <b>primeira vez em<br>72 anos</b> o número de<br>mulheres ultrapassa o<br>número de homens:<br>',
                   "<span style='color:#0081C8;'><b>158</span>", ' x ', "<span style='color:#EE334E;'><b>160</span>")),
  x = c(1901, 1932, 1996, 2011),
  y = c(92, 217, 242, 80)
)

map_segm = data.frame(
  x = c(1900, 1932, 1996),
  xend = c(1900, 1932, 1996),
  y = c(0, 3, 69),
  yend = c(70, 195, 220)
)





## titulo 
title = paste0("Participação brasileira ", 
               "<span style='color:#BC4B51;'><b>feminina</span>", 
               " x ", 
               "<span style='color:#0081C8;'><b>masculina</span>", 
               " nos Jogos Olímpicos")




## plot
ggplot(participantes) +
  geom_line(aes(x=Ano, y=Esporte, group=Sexo, color=Sexo), 
            linewidth=1.1) +
  scale_color_manual(values=c('#BC4B51', '#0081C8')) +
  geom_segment(data = map_segm,
               aes(x=x, xend=xend, y=y, yend=yend),
               color='#BC4B51',
               linetype='dashed') +
  geom_richtext(data = map_text,
                aes(x=x, y=y, label=label),
                size=11,
                family='quicksand',
                color='#444455',
                lineheight=.5,
                label.color=NA,
                fill=NA) +
  geom_image(data = map_img,
             aes(x=x, y=y, image=img),
             size=0.07,
             asp=1.8) +
  geom_point(aes(x=2004, y=159),
             shape=21,
             color='#444455',
             size=15) +
  geom_curve(aes(x=2006, xend=2012, y=158, yend=110),
             linewidth=0.3,
             curvature=-0.3,
             arrow=arrow(length=unit(0.1,"in")),
             color='#444455') +
  scale_x_continuous(breaks=seq(1896, 2016, 4)) +
  scale_y_continuous(breaks=seq(0, 300, 50)) +
  labs(title=title, 
       x='Ano', y='Número de participantes') +
  theme_minimal() +
  theme(plot.background = element_rect(fill='#F5F3F4', color='#F5F3F4'),
        plot.title = element_markdown(size=60,
                                      family='quicksand',
                                      color='#444455',
                                      hjust=.2,
                                      margin=margin(t=20)),
        axis.title = element_text(size=25,
                                  family='quicksand',
                                  color='#444455'),
        axis.text.x = element_text(size=23,
                                   face='bold',
                                   family='quicksand',
                                   margin=margin(b=10)),
        axis.text.y = element_text(size=20,
                                   family='quicksand',
                                   margin=margin(l=10)),
        panel.grid = element_blank(),
        legend.position = 'none')





## salvando
ggview(units='px', height=3000, width=4900)            
ggsave(units='px', height=3000, width=4900, filename='participacao_feminina.png')
