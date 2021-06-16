#!/usr/bin/python
# -*- coding: UTF-8 -*-

from bs4 import BeautifulSoup
import requests
import re


# Data
data= []

def get_links(URL):
    for i in range(1, 4):
        url = (URL + '?page=' + str(i))
        data.append(url)
        print()

get_links('https://www.falabella.com/falabella-cl/category/cat20002/Moda-Mujer')


# las paginas de fallabella a las cuales se le van a sacar los links de productos
for link in data:
    page = requests.get(link)
    soup = BeautifulSoup(page.content, "html.parser")

    for a_href in soup.find_all("a",
                                attrs={'href': re.compile("^https://www.falabella.com/falabella-cl/product/")}):
        with open("falabella_links.txt", "a") as linkfile:
            linkfile.write(a_href["href"] + "\n")