readxl::read_excel("data_dec/water_stress.xlsx")
d1 <- readxl::read_excel("data_dec/water_stress.xlsx",sheet = "Data")
ggplot(d1, aes(x = v, y = b)) +
  geom_point()

sp1 <- paste("d1", 1:252, sep="")
print(sp1)

sp2 <- d1[1:252, ]
print(sp2)

n <- 