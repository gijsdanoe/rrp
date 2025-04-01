# rrp
ABRZNN Regionaal Resistentieprofiel

In deze repository zit de code voor de pipeline van de data en het dashboard ontwikkeld door het projectteam 'Regionaal Resistentieprofiel' van het Antibioticaresistentie Netwerk Noord-Nederland om de huidige status en ontwikkeling van antibioticaresistentie en BRMO's in Noord-Nederland in kaart te brengen. De resistentiedata van Certa, Izore en UMCG is niet publiek toegankelijk.

De volgende bestanden kun je hier vinden:

Code:
- pipeline.r: de pipeline voor het combineren en opschonen van de data
- dashboard.r: de code voor het R Shiny dashboard

Data:
- postcodes.rds: geografische referentiedata
- mtrlgroepen.csv: materiaalgroepen van elke code gebruikt door Certe, Izore en UMCG

Bekijk het dashboard online op: https://resistentieprofiel.azurewebsites.net/
