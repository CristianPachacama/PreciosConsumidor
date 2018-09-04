#=================    PUBLICACION EN SHINY SERVER    =======================
getwd()
# Aplicacion de TS Cluster IPC --------
rsconnect::setAccountInfo(name='cristianpachacama',
                          token='7D40FA1526E25B058EA1FD093427FBB0',
                          secret='8aIg5VQbxh9fDq3mh8bxLxO9GXoPWHMGF57a+5IG')

rsconnect::deployApp("App Cluster IPC",
                     account = "cristianpachacama",
                     appName = "ClusterIPC")

# Aplicacion de Regresion Log IPC --------
rsconnect::setAccountInfo(name='cristianpachacama',
                          token='7D40FA1526E25B058EA1FD093427FBB0',
                          secret='8aIg5VQbxh9fDq3mh8bxLxO9GXoPWHMGF57a+5IG')

rsconnect::deployApp("App Regresion IPC",
                     account = "cristianpachacama",
                     appName = "RegresionIPC")
