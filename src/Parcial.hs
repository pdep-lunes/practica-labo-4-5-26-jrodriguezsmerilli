module Parcial where
import Text.Show.Functions()

type Raza = String
type Juguete = String
type Tiempo = Int
type Energia = Int
type Nombre = String

data Perro = UnPerro {
  raza::Raza,
  juguetesFavoritos::[Juguete],
  tiempoEnGuarderia::Tiempo,
  energia::Energia
}

data Guarderia = UnaGuarderia{
  nombre::Nombre,
  rutina::[((Perro->Perro),Tiempo)]
}

razasExtravagantes :: [Raza]
razasExtravagantes = ["Dalmata", "Pomeriana"]

sacarPrimerElemento::[String] -> [String]
sacarPrimerElemento unaLista = drop 1 unaLista

modificarEnergia::(Int->Energia) -> Perro -> Perro
modificarEnergia unaFuncion unPerro = unPerro{energia = (max 0).unaFuncion.energia $ unPerro}
esDeRazaExtravagante::[Raza]->Raza->Bool
esDeRazaExtravagante listaDeRazas unaRaza = any (esDeRaza unaRaza) listaDeRazas
esDeRaza::Raza->Raza->Bool
esDeRaza unaRaza otraRaza = unaRaza == otraRaza
mereceSpa::[Raza]->Perro->Bool
mereceSpa listaDeRazas unPerro = ((esDeRazaExtravagante listaDeRazas).raza) unPerro || tiempoEnGuarderia unPerro >= 50



jugar::Perro -> Perro
jugar unPerro = modificarEnergia (+ (-10)) unPerro
ladrar::Int->Perro->Perro
ladrar cantLadridos unPerro = modificarEnergia (+ (div cantLadridos 2)) unPerro
regalar::Juguete->Perro->Perro
regalar unJuguete unPerro = unPerro{juguetesFavoritos= juguetesFavoritos unPerro ++ [unJuguete]}
diaDeSpa::[Raza]->Perro->Perro
diaDeSpa listaDeRazas unPerro
 |mereceSpa listaDeRazas unPerro = regalar "Peine de Goma" (unPerro{energia = 100})
 |otherwise = unPerro
diaDeCampo::Perro -> Perro
diaDeCampo unPerro = jugar (unPerro{juguetesFavoritos = sacarPrimerElemento.juguetesFavoritos unPerro})


puedeEstarEnLaGuarderia::Guarderia->Perro->Bool
puedeEstarEnLaGuarderia unaGuarderia unPerro = tiempoRutina unaGuarderia < tiempoEnGuarderia unPerro
tiempoRutina:: Guarderia -> Tiempo
tiempoRutina unaGuarderia = sum.(map tiempoActividad) $ rutina unaGuarderia
tiempoActividad::((Perro->Perro), Tiempo) -> Tiempo
tiempoActividad (_, tiempo) = tiempo

esPerroResponsable::Perro->Bool
esPerroResponsable unPerro = (length.juguetesFavoritos.diaDeCampo) unPerro > 3

zara::Perro
zara = UnPerro{
  raza="Dalmata",
  juguetesFavoritos=["Pelota","Mantita"],
  tiempoEnGuarderia=90,
  energia=80
}

guarderiaPdePerritos::Guarderia
guarderiaPdePerritos = UnaGuarderia{
  nombre="GuarderiaPdePerritos",
  rutina=[(jugar, 30), (ladrar 18, 20), (regalar "pelota", 0), (diaDeSpa razasExtravagantes, 120), (diaDeCampo, 720)]
}

