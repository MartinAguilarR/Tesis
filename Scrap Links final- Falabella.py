from bs4 import BeautifulSoup
import requests
import re


# Data
data= []

def get_links(URL):
    for i in range(1, 201):
        url = (URL + '?page=' + str(i))
        data.append(url)
        print()

get_links('https://www.falabella.com/falabella-cl/category/cat20002/Moda-Mujer')

# nose como guardar los links en una variable o lista asi que los escribi
    # Martín : Lo que se puede hacer es transformar esto a un data frame.
    # en el paso anterior se hace un listado y se le agregan los elementos de la iteración.

# archive_links = ["https://www.falabella.com/falabella-cl/category/cat20002/Moda-Mujer?page=1",
#                 "https://www.falabella.com/falabella-cl/category/cat20002/Moda-Mujer?page=2",
#                 "https://www.falabella.com/falabella-cl/category/cat20002/Moda-Mujer?page=3"]


# las paginas de fallabella a las cuales se le van a sacar los links de productos
for link in data:
    page = requests.get(link)
    soup = BeautifulSoup(page.content, "html.parser")

    for a_href in soup.find_all("a",
                                attrs={'href': re.compile("^https://www.falabella.com/falabella-cl/product/")}):
        with open("falabella_links.txt", "a") as linkfile:
            linkfile.write(a_href["href"] + "\n")