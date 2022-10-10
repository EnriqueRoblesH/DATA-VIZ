df <- read_excel("Downloads/PPOILUSDM.xls")
date<-df$observation_date
df1 <- df %>%
  mutate(Date1 = as.Date(date, format = "%d.%m.%Y")) %>%
  mutate(pice = as.numeric(df$PPOILUSDM)) %>%
  group_by(month_date = floor_date(Date1, "month"))


df1 %>%
  ggplot(aes(x=month_date)) +
  geom_rect(xmin=as.Date("2022-01-01"), xmax=as.Date("2022-08-01"), ymin=10, ymax=1750, fill='#56B4E9', alpha=0.01) +
  geom_path(aes(y=price), size=1, color="#009E73") +
  geom_textbox(
    data = tribble(
      ~x, ~y, ~label,
      as.Date("2016-07-01"), 1500, "Precio máximo desde 1990 (originado por guerra de Ucrania- Rusia, aumento del precio del aceite de girasol; efecto sustitución.)",
      
    ),
    aes(x=x,y=y,label=label),
    size=4, family='Georgia', box.colour = 'white', face="italic", color="#009E73"
  ) +
  geom_curve(
    data = tribble(
      ~x1,~x2,~y1,~y2,
      as.Date("2018-11-01"), as.Date("2022-02-01"), 1500,1640.71
    ),
    aes(x=x1,xend=x2,y=y1,yend=y2),
    arrow = arrow(length = unit(5,'points')),
    curvature = 0.5,
    color = 'red',
    size = 2
  )+
  
  scale_colour_gradient2(low='#E3120B', mid='#E3EBF0', high='#3E51B5') +
  labs(title = "<br><span style='color:#009E73'><b>Aceite de palma</b></span>",
       subtitle = "Datos  históricos  del  aceite  de  palma  desde  1990.<br><span style='color:#56B4E9'><b>Periodo desde el inicio del conflicto entre Rusia y Ucrania</b></span>",
       caption = "Fuente: Federal Reserve Economic Data <br> Autor: Enrique Robles  (@enriquerh_)") +
  scale_y_continuous() +
  theme(
    plot.title = element_textbox_simple(size=50, margin = margin(-50,0,0,0), color='#121212', face = 'bold', family='Georgia'),
    plot.subtitle = element_textbox(size=20, margin = margin(20,0,10,0), color = '#121212', face = 'italic', family='Georgia'),
    plot.caption = element_textbox_simple(size=10, family='Georgia', face='italic', color='gray40'),
    legend.position = 'top',
    plot.title.position = 'plot',
    plot.background = element_rect(fill = 'white', color = 'white'),
    panel.background = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.text.x = element_text(face='bold', size= 10, family='Georgia'),
    axis.text.y = element_text(face='bold', size= 10, family='Georgia'),
    panel.grid.major.y = element_line(size=0.5, color = '#D7D7D7'),
  )+
  theme(panel.border = element_rect(color = "#009E73",
                                    fill = NA,
                                    size = 3))
