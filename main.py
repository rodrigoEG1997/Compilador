import ply.lex as lex
import ply.yacc as yacc
import sys
import maquinaV

tokens = [
    'Programa',
    'Identificador',
    'PuntoComa',
    'Principal',
    'BraquetIzq',
    'BraquetDer',
    'Var',
    'Tipo',
    'CorcheteIzq',
    'CorcheteDer',
    'ConstanteInt',
    'ConstanteFloat',
    'ConstanteChar',
    'Coma',
    'Funcion',
    'TipoRetorno',
    'ParentecisIzq',
    'ParentecisDer',
    'Igual',
    'SignoSumRest',
    'SignoMultDiv',
    'SignoLogico',
    'OperadorLogico',
    'Lee',
    'Escribe',
    'Letrero',
    'Si',
    'Entonces',
    'Sino',
    'Mientras',
    'Haz',
    'Desde',
    'Hasta',
    'Hacer',
    'Regresa'
    ]

reserved = {
    'programa'      :   'Programa',
    'principal'     :   'Principal',
    'var'           :   'Var',
    'funcion'       :   'Funcion',
    'lee'           :   'Lee',   
    'escribe'       :   'Escribe',
    'si'            :   'Si',
    'entonces'      :   'Entonces',
    'sino'          :   'Sino',
    'mientras'      :   'Mientras',
    'haz'           :   'Haz',
    'desde'         :   'Desde',
    'regresa'       :   'Regresa',
    'hasta'         :   'Hasta',
    'hacer'         :   'Hacer',
    'int'           :   'Tipo',
    'float'         :   'Tipo',
    'char'          :   'Tipo',
    'void'          :   'TipoRetorno'
    }


t_PuntoComa         =   r';'
t_Coma              =   r','
t_BraquetIzq        =   r'\{'
t_BraquetDer        =   r'\}'
t_CorcheteIzq       =   r'\['
t_CorcheteDer       =   r'\]'
t_ParentecisIzq     =   r'\('
t_ParentecisDer     =   r'\)'
t_Igual             =   r'\='
t_SignoSumRest      =   r'\+ | \-'
t_SignoMultDiv      =   r'\* | \/'
t_SignoLogico       =   r'\& | \|'
t_OperadorLogico    =   r'\=\= | \<\= | \>\= | \< | \>'
t_Letrero           =   r'\'[a-zA-Z0-9 \.\?\:\t\r\n\f()\[\]\&\!\@\#\$\%\^\-\=\+\/\,]*\''
t_ignore            =   ' \t\r\n\f\v'


def t_Identificador(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    if t.value in reserved:
        t.type = reserved[ t.value ]
    else:  
        t.type = 'Identificador'
    return t

def t_ConstanteFloat(t):
    r'[-]?\d+\.\d+'
    t.value = float(t.value)
    return t

def t_ConstanteInt(t):
    r'[-]?[0-9][0-9]*'
    t.value = int(t.value)
    return t

def t_ConstanteChar(t):
    r'\".*?\"'
    t.value = str(t.value)
    return t


def t_error(t):
    print("Caracter ilegal!")
    t.lexer.skip(1)


lexer = lex.lex()

#Declaracion de variables globales
funcionActual = 'global' #Guarda la funcion actua (saber donde referirce a las variables)
tablaVariables = {'global':{'tipoFuncion':'global','nombreFuncion':'global','variables':{}}} #Tabla de funciones y variables
banderVariable = True #para las comas en las declaraciones
tipoVariable = None #Guarda el tipo de la variable declarada (en caso de haber comas)
listaOpeLogicos = [] #lista de operadores logicos, para las expreciones
listaVarLogicos = [] #lista de variables logicas, pata las expreciones
listaVO = {'0': {'operador': [], 'variables': []}} #Lista de listas para las expreciones arismeticas (poder hacer parentecis)
contListasVO = 0 #llevar el contador de las listas
cuadruplos = [] #Se guardan los cuadruplos
contCuadruplos = 0 #Contador de cuadruplos (para poder manipular los goto)
contVarTemp = 0 #contador de variables temporales
varTemp =[] #lista de variables temporales
listaGoto = [] #lista para saber donde su pusieron goto y poder regresar
memoriaDisponible = [] #memoria del programa
constantes = [] #guarda constantes
nombreFuncion = [] #para las llamada a funcion, saber en cual funcion estan
contParametros = 0 #validar que tenga exactamente los parametros que se declararon
limiteArray = None #guarda le limite del ultimo array leido
regreso = False #valid que el ultimo estatuto sea regresa
listaDir = [] #guarda las variables apuntadores
varConst = {} #guarda todas las constantes con su memoria (para la maquina virtual)
memoriaFunciones = {} #guarda las variables y su memoria de las funciones (por que se borran) para la amaquina virtual
contMemFuncion = 0 #contador de varaible 
funcionParametros = 'global' #No se usa
lastDir = {'nombre': None, 'memoria': None} #Encontrar cual es la ultima direccion y su memoria
listaApuntador = {}

#Crea los espacios de memoria
for i in range(14000):
    memoriaDisponible.append(i)

#Direcciones de memoria
"""
#Global
0 - 999 int
1000 - 1999 float
2000 - 2999 char
#Local
3000 - 3999 int
4000 - 4999 float
5000 - 5999 char
---
6000 - 8999 const
9000 - 11999 temp
12000 - 13999 dir
"""

#Comienza la sintaxis

def p_programa(p):
    '''
    programa            : Programa Identificador goto PuntoComa programaGlobal Principal popGoto ParentecisIzq ParentecisDer BraquetIzq estatutoVoid BraquetDer addEndProgram llamarMaquinaVirtual
    '''
    """
    for i in tablaVariables:
        for j in tablaVariables[i]['variables']:
            print(tablaVariables[i]['variables'][j])
        #print(tablaVariables[i])
        print("\n")
    print(listaApuntador)
    """
    #print(listaVO)
    #print(cuadruplos[contCuadruplos-1].split())
    
    
    """
    print(listaDir)
    print(memoriaDisponible)
    """


#Se manda llamar la maquina virtual. Los cuadruplos tienen que estar finalizados
#varConst contiene TODAS las constantes que se detectaron y su respectiva memoria
#memoriaFunciones tiene todas las variables locales con su memoria, separadas por su respectiva funcion
def p_llamarMaquinaVirtual(p):
    "llamarMaquinaVirtual :"
    maquinaV.virtual(cuadruplos,varConst,memoriaFunciones,listaApuntador)
    

def p_programaGlobal(p):
    '''
    programaGlobal      : variables definirFuncion programaGlobal
                        | variables 
                        | definirFuncion programaGlobal
                        | 
    '''

def p_variables(p):
    '''
    variables           : Var definirTipoVariable
    '''



def p_definirTipoVariable(p):
    '''
    definirTipoVariable : Tipo definirVariable PuntoComa 
                        | Tipo definirVariable PuntoComa definirTipoVariable
    '''

def p_definirVariable(p):
    '''
    definirVariable     : tipoIdentificadorDeclaracion defineTipo
                        | tipoIdentificadorDeclaracion Coma defineTipo definirVariable
    '''
    global banderVariable
    banderVariable = True
    p[0] = p[1]

def p_tipoIdentificadorDeclaracion(p):
    '''
    tipoIdentificadorDeclaracion    : Identificador
                                    | arrayDeclaracion    
    '''
    p[0] = p[1]

def p_arrayDeclaracion(p):
    '''
    arrayDeclaracion    : Identificador CorcheteIzq ConstanteInt validarInt CorcheteDer
    '''
    global limiteArray

    p[0] = p[1]
    limiteArray = p[3]


def p_tipoIdentificador(p):
    '''
    tipoIdentificador   : Identificador validarIdentificador
                        | array
    '''
    
    p[0] = p[1]

#Checa que si es una variable sin dimenciones, verdaderamente no tenga dimenciones
def p_validarIdentificador(p):
    "validarIdentificador :"

    func = validarVar(p[-1])
    if tablaVariables[func]['variables'][p[-1]]['limites'] != None:
        print("ERROR, Arreglo sin dimenciones")
        sys.exit(1)

def p_array(p):
    '''
    array               : Identificador CorcheteIzq addLista asignarValor cerrarListaArray verArray CorcheteDer
    '''
    p[0] = p[1]

#quita el contador de contListasVO sin dar ningun valor de retorno
def p_cerrarListaArray(p):
    "cerrarListaArray :"
    global contListasVO

    if contListasVO > 0:
        del listaVO[str(contListasVO)]
        contListasVO -= 1



#Crea los cuadruplos Ver, y las dos sumas que se necesitan realizar para calcular la dimencion de arreglo
def p_verArray(p):
    "verArray :"
    global contCuadruplos
    global contVarTemp
    global lastDir

    funcLim = validarVar(p[-2])
    limite = encontrarLimite(p[-5])
    memLimite = encontrarMemoria(limite)
    memCero = encontrarMemoria(0)
    memParArr = encontrarMemoria(p[-2])
    tipoMemPar = None
    

    #En caso de que el parametro de un arreglo sea otro arreglo no poner la memoria ya que es un apuntador
    if tablaVariables[funcLim]['variables'][p[-2]]['limites'] != None: #checa si es una variable dimencionada
        memParArr = lastDir['memoria']

        encontrado = False #indica si encontro la varable o no

        #encuentra el tipo de variable que es el arreglo ya que se tiene una variable apuntador que no tiene tipo

        #busca la variable en la funcion actual
        for i in tablaVariables[funcionActual]['variables']:                                    
                if tablaVariables[funcionActual]['variables'][i]['direccion'] == memParArr:     
                    tipoMemPar = tablaVariables[funcionActual]['variables'][i]['tipoVariable']
                    encontrado = True
                    break
        
        #busca en global
        if encontrado == False:
            for i in tablaVariables['global']['variables']:
                if tablaVariables['global']['variables'][i]['direccion'] == memParArr:
                    tipoMemPar = tablaVariables['global']['variables'][i]['tipoVariable']
                    break
        
        if tipoMemPar == None:
            print('ERROR, No se encontro tipo variable de apuntador arreglo')
            sys.exit(1)
    else:
        tipoMemPar = tablaVariables[funcLim]['variables'][p[-2]]['tipoVariable']

    #valida que el parametro del arreglo sea int
    if tipoMemPar != 'int':
        print('ERROR, parametro de arreglo diferente a int')
        sys.exit(1)
    
    #agrega el cuadruplo Ver
    cuadruplos.append('Ver ' + str(memParArr) + ' ' + str(memCero) + ' ' + str(memLimite))
    contCuadruplos += 1
    
    #agrega la variable 0 si no esta declarada previamente ya que es por default el limite inferior
    addConst(0)
    
    funcDer = validarVar(0)

    tipo_Der = tablaVariables[funcDer]['variables'][0]['tipoVariable']
    
    tipo = designer('+',tipoMemPar,tipo_Der)
    temp = addVarTemp(tipo)
    
    memoria_Der = tablaVariables[funcDer]['variables'][0]['memoria']
    memoria_temp = tablaVariables[funcionActual]['variables'][temp]['memoria']
    
    #siempre va a sumarle 0 por como funciona el programa, (esta por simple estandar)
    cuadruplos.append('+ ' + str(memParArr) + ' ' + str(memoria_Der) + ' ' + str(memoria_temp))
    contCuadruplos += 1
    contVarTemp += 1

    memArr = encontrarMemoria(p[-5])

    #agrega la constante que va a tener la direccion de memoria del arreglo
    addConst(memArr)

    lastTemp = varTemp[len(varTemp)-1]['memoria']
    memMemArr = encontrarMemoria(memArr)

    direccion = addVarDireccion(memoria_Der)
    memDireccion = tablaVariables[funcionActual]['variables'][direccion]['memoria']

    funArray = validarVar(p[-5])

    tablaVariables[funArray]['variables'][p[-5]]['direccion'] = memDireccion
    
    #cuadruplo para tener la variable apuntador que va a hacer referencia al arreglo
    cuadruplos.append('+ ' + str(lastTemp) + ' ' + str(memMemArr) + ' ' + str(memDireccion))
    contCuadruplos += 1

#Crea las variables de tipo apuntador
def addVarDireccion(memoria_Der):
    global lastDir
    
    #Encuentra su memoria
    num = memDir()

    #Crea la variable en la table de variables determinada
    tablaVariables[funcionActual]['variables'][str('#P'+str(num-12000))] = {'nombreVariable':str('#P'+str(num-12000)),'tipoVariable':'pointer','memoria': num, 'limites':None, 'direccion':None}
    lastDir['nombre'] = str('#P'+str(num-12000))
    lastDir['memoria'] = num
    listaApuntador[num] = memoria_Der

    #regresa el nombre de la variable
    return str('#P'+str(num-12000))
    
#Encuentra la memoria de las variables
def encontrarMemoria(num):
    #checa en la funcion actual
    if num in tablaVariables[funcionActual]['variables']:
        return tablaVariables[funcionActual]['variables'][num]['memoria']
    #checa en la las variables globales
    if num in tablaVariables['global']['variables']:
        return tablaVariables['global']['variables'][num]['memoria']
    
    print("ERROR, No se encontro memoria de variable")
    sys.exit(1)

def encontrarLimite(arr):
    if arr in tablaVariables[funcionActual]['variables']:
        return tablaVariables[funcionActual]['variables'][arr]['limites']
    if arr in tablaVariables['global']['variables']:
        return tablaVariables['global']['variables'][arr]['limites']
    
    print("ERROR, No se encontro limite de arreglo")
    sys.exit(1)


#Al final llevan popVars las dos filas
def p_definirFuncion(p):
    '''
    definirFuncion      : Funcion TipoRetorno Identificador modificarFuncionActual ParentecisIzq parametrosDeclaracion ParentecisDer variablesLocales BraquetIzq estatutoVoid BraquetDer addEndFunc popVars
                        | Funcion Tipo Identificador modificarFuncionActual ParentecisIzq parametrosDeclaracion ParentecisDer variablesLocales BraquetIzq estatuto validarRegresa BraquetDer popVars
    '''
    global funcionActual
    global contMemFuncion

    funcionActual = 'global'
    contMemFuncion = 0

def p_validarRegresa(p):
    "validarRegresa :"
    global regreso

    if regreso != True:
        print("ERROR, Falta valor de regreso de funcion")
        sys.exit(1)
    regreso = False

#Verifica si la funcion no esta previamente declarada y la guarda
def p_modificarFuncionActual(p):
    "modificarFuncionActual :"
    global funcionActual
    global contCuadruplos
    global memoriaFunciones

    if p[-1] not in tablaVariables:
        funcionActual = p[-1]
        #crea el espacio de memoria para la funcion
        tablaVariables[funcionActual] = {'tipo':p[-2],'nombreFuncion':funcionActual,'memoria': contCuadruplos,'tiposParametros': [],'memoriaParametros': [], 'variables':{}}
        if p[-2] != 'void':
            num = memGlobal(p[-2])
            #guarda como variable global el nombre de esta funcion para el return
            tablaVariables['global']['variables'][p[-1]] = {'nombreVariable':p[-1],'tipoVariable':p[-2],'memoria': num, 'limites':None, 'direccion':None}
    else:
        print("Error al modificar funcion")
        return p_error(p)
    
    memoriaFunciones[funcionActual] = {} #agrega el espacio de memoria de esta funcion para guradar todas las variables para la mnaquina virtual

def p_parametrosDeclaracion(p):
    '''
    parametrosDeclaracion   : parametros
                            |
    '''

def p_parametros(p):
    '''
    parametros          : Tipo Identificador addVarParametro 
                        | Tipo Identificador addVarParametro Coma parametros
    '''

#Se leen las variables que se van a utilizar y se agregan a 
def p_addVarParametro(p):
    "addVarParametro :"

    tipoVariable = p[-2]
    nombreVal = p[-1]

    #se agrega el tipo a la tabla de funcion (tienen que estar ordenado obligatoriamente)
    tablaVariables[funcionActual]['tiposParametros'].append(str(p[-2]))

    #se valida que no se repitan los parametros
    if nombreVal not in tablaVariables[funcionActual]['variables']:
        #se busca una memoria para esa variable
        if funcionActual == 'global':
            num = memGlobal(tipoVariable)
        else:
            num = memLocal(tipoVariable)
        
        if num == None:
            print("Error, No hay espacio de memoria para la variabe")
            sys.exit(1)
        #se agrega la variable a la tabla de variables
        tablaVariables[funcionActual]['variables'][nombreVal] = {'nombreVariable':nombreVal,'tipoVariable':tipoVariable,'memoria': num, 'limites':None, 'direccion':None}
        #se agrega la memoria del parametro en la tabla de funciones
        tablaVariables[funcionActual]['memoriaParametros'].append(str(num))
    else:
        print('ERROR, PARAMETROS CON MISMO NOMBRE')
        sys.exit(1)

def p_variablesLocales(p):
    '''
    variablesLocales    : variables 
                        | 
    '''
def p_estatuto(p):
    '''
    estatuto            : asignacion noReturn estatuto 
                        | lectura noReturn estatuto 
                        | escritura noReturn estatuto 
                        | si noReturn estatuto 
                        | mientras noReturn estatuto 
                        | desde noReturn estatuto 
                        | llamarFuncion noReturn estatuto 
                        | regresa siReturn addEndFunc estatuto
                        | 
    '''

#Cambia bander de regresa
def p_noReturn(p):
    "noReturn :"
    global regreso
    regreso = False

def p_siReturn(p):
    "siReturn :"
    global regreso
    regreso = True

def p_estatutoVoid(p):
    '''
    estatutoVoid        : asignacion estatuto 
                        | lectura estatuto 
                        | escritura estatuto 
                        | si estatuto 
                        | mientras estatuto 
                        | desde estatuto 
                        | llamarFuncion estatuto 
                        | 
    '''

def p_asignacion(p):
    '''
    asignacion          : tipoIdentificador validarVariable iniciarListaDir Igual asignarValor validarDirArray PuntoComa
    '''
    global contCuadruplos

    #Crea el cuadriplo de asignacion

    #Valida si las variables estan en la lista de variables (tanto global como local)
    variable = validarVar(p[1])
    valor = validarVar(p[5])

    #Encuentra la memoria de la variable
    var = encontrarMem(variable,p[1])
    val = encontrarMem(valor,p[5])

    #Valida wue no sea arreglo, y en caso de serlo se trae la memoria del apuntador
    val = encontrarDir(val)
    var = encontrarDir(var)

    #Encuentra el tipo de las variables
    tipoVar = tablaVariables[variable]['variables'][p[1]]['tipoVariable']
    tipoValor = tablaVariables[valor]['variables'][p[5]]['tipoVariable']

    #Valida si son compatibles
    tipo = designer('=',tipoVar,tipoValor)
    if tipo == 'err':
        print('ERROR, Variables incompatibles')
        sys.exit(1)

    #Crea el cuadruplo de asignacion 
    cuadruplos.append(str(p[4]) + ' ' + str(val) + ' ' + str(var))
    contCuadruplos += 1

#valida que sea un arreglo (osea que tenga limites)
def p_validarDirArray(p):
    "validarDirArray :"

    func = validarVar(p[-1])

    #pregunta si tiene limites
    if tablaVariables[func]['variables'][p[-1]]['limites'] != None:
        listaDir.append(tablaVariables[func]['variables'][p[-1]]['direccion'])

#inicia la lista de direcciones (sirve para poder saber cual fue la ultima direccion utilizada)
def p_iniciarListaDir(p):
    "iniciarListaDir :"
    global listaDir

    func = validarVar(p[-2])

    if tablaVariables[func]['variables'][p[-2]]['limites'] != None:
        listaDir.append(tablaVariables[func]['variables'][p[-2]]['direccion'])

  
def p_asignarValor(p):
    '''
    asignarValor        : exprecionArismetica valorFinal
                        | tipoIdentificador validarVariable 
                        | llamarFuncionAsignacion validarLlamada
                        | ConstanteInt validarInt
                        | ConstanteFloat validarFloat
                        | ConstanteChar validarChar
    '''
    if p[1] != None:
        p[0] = p[1]
    else:
        p[0] = p[2]

#Se pone despues de una llamada a funcion, y es por que siempre el resultado siempre se guarda en una variable temporal,
#por lo tanto, solamente se encarga de regresar el valor de que regresa la funcion
def p_validarLlamada(p):
    "validarLlamada :"
    p[0] = varTemp[len(varTemp) - 1]['nombreVariable']

#Validar que la variable identificador ya este declarada precviamente
def p_validarVariable(p):
    "validarVariable :"
    global contMemFuncion

    #validar en las variables globales y locales
    if p[-1] not in tablaVariables['global']['variables']:
        if p[-1] not in tablaVariables[funcionActual]['variables']:
            print('Error, variable no declarada')
            sys.exit(1)

    #Si la variable esta declarada en las globales y no en la funcion actual, se guarda tambien en la funcion actual
    if funcionActual != 'global' and p[-1] in tablaVariables['global']['variables']:
        mem = tablaVariables['global']['variables'][p[-1]]['memoria']
        memoriaFunciones[funcionActual][mem] = contMemFuncion
        contMemFuncion += 1
 

def p_llamarFuncion(p):
    '''
    llamarFuncion       : Identificador validarFuncion ParentecisIzq parametrosLlamada goSub asignarValorFuncion ParentecisDer PuntoComa
    '''

def p_llamarFuncionAsignacion(p):
    '''
    llamarFuncionAsignacion       : Identificador validarFuncion ParentecisIzq parametrosLlamada goSub asignarValorFuncion ParentecisDer
    '''

#Se asigna el 'parche' para tener el valor que regreso la funcion en la variable global de esta respectiva funcion
def p_asignarValorFuncion(p):
    "asignarValorFuncion :"
    global contVarTemp
    global contCuadruplos

    if p[-5] in tablaVariables['global']['variables']:
    
        #encuentra la memoria de la funcion en las variables globales
        memFunc = encontrarMem('global',p[-5])
        #encuentra el tipo de la funcion
        tipoFunc = tablaVariables['global']['variables'][p[-5]]['tipoVariable']

        #crea la variable temporal
        temp = addVarTemp(tipoFunc)
        memoria_temp = tablaVariables[funcionActual]['variables'][temp]['memoria']
        
        #crea el cuadruplo para asignarle el regreso de la funcon a un temporal
        cuadruplos.append('= ' + str(memFunc) + ' ' + str(memoria_temp))
        contCuadruplos += 1
        contVarTemp += 1

#Se agrega el cuadruplo goSub
def p_goSub(p):
    "goSub :"
    global contParametros
    global contCuadruplos

    #Busca el nombre de la funcion que se esta llamando (es la ultima de la lista)
    nombre = nombreFuncion[len(nombreFuncion)-1]
    if contParametros != len(tablaVariables[nombre]['tiposParametros']):
        print('Error, numero incorecto de parametros')
        sys.exit(1)

    #Saca el ultimo valor ya que no se va a utilizar  por que ya termino de leer los parametros
    nombre = nombreFuncion.pop()
    #cual es el cuadruplo en el que empiezan las instrucciones de la funcion
    mem = tablaVariables[nombre]['memoria']

    #se crea el cuadruplo
    cuadruplos.append('GoSub ' + str(mem))
    contCuadruplos += 1
    #Se inicializa en 0 el contador de parametros para la proxima llamada a funcion
    contParametros = 0

#Valida que la funcion a llamar si este declarada en la tabla de funciones y crea el cuadruplo ERA
def p_validarFuncion(p):
    "validarFuncion :"
    global contCuadruplos
    #global funcionParametros

    if p[-1] not in tablaVariables:
        print(p[-1])
        print('Error, funcion no identificada')
        sys.exit(1)
    #crea el cuadruplo era
    cuadruplos.append('ERA ' + str(p[-1]))
    contCuadruplos += 1
    #para saber a que funcion es la que se esta llamdo y sacar el tipo y numero de los parameros
    nombreFuncion.append(str(p[-1]))

def p_parametrosLlamada(p):
    '''
    parametrosLlamada   : valoresLlamada
                        |  
    '''

def p_valoresLlamada(p):
    '''
    valoresLlamada      : addLista asignarValor paramFuncion cerrarListaArray
                        | addLista asignarValor paramFuncion cerrarListaArray Coma valoresLlamada
    '''

#Lee la memoria que va aser asignada a el parametro
def p_paramFuncion(p):
    "paramFuncion :"
    global contCuadruplos
    global contParametros

    #Encuentra el nombre de la funcion que se esta llamando (para poder hacer todas las validaciones)
    nombre = nombreFuncion[len(nombreFuncion)-1]

    #Valida que todavia queden parametros por leer
    if contParametros >= len(tablaVariables[nombre]['tiposParametros']):
        print('Error, numero incorecto de parametros')
        sys.exit(1)

    #Encuentra el valor de memoria y crea el cuadruplo Param

    #Checa si esta en la funcion actual
    if p[-1] in tablaVariables[funcionActual]['variables']:
        #Valida que el valor y el tipo del parametro sean iguales
        if tablaVariables[funcionActual]['variables'][p[-1]]['tipoVariable'] == tablaVariables[nombre]['tiposParametros'][contParametros]:
            #Checa si lo qeu es esta mandando es un apuntador
            if tablaVariables[funcionActual]['variables'][p[-1]]['limites'] != None:
                memoriaVar = lastDir['memoria']
            else:
                memoriaVar = tablaVariables[funcionActual]['variables'][p[-1]]['memoria']
            memPara = tablaVariables[nombre]['memoriaParametros'][contParametros]
            #Crea el cuadruplo
            cuadruplos.append('Param ' + str(memoriaVar) + ' ' + memPara)
            contCuadruplos += 1
        else:
            print('Error, Tipo de parametro no compatible')
            sys.exit(1)
    #Checa si la variable esta en las globales
    elif p[-1] in tablaVariables['global']['variables']:
        #Valida que el valor y el tipo del parametro sean iguales
        if tablaVariables['global']['variables'][p[-1]]['tipoVariable'] == tablaVariables[nombre]['tiposParametros'][contParametros]:
            #Checa si lo qeu es esta mandando es un apuntador
            if tablaVariables['global']['variables'][p[-1]]['limites'] != None:
                memoriaVar = lastDir['memoria']
            else:
                memoriaVar = tablaVariables['global']['variables'][p[-1]]['memoria']
            memPara = tablaVariables[nombre]['memoriaParametros'][contParametros]
            #Crea el cuadruplo
            cuadruplos.append('Param ' + str(memoriaVar) + ' ' + memPara)
            contCuadruplos += 1
        else:
            print('Error, Tipo de parametro no compatible')
            sys.exit(1)
    else:
        print(p[-1])
        print('ERROR, variable no identificada')
        sys.exit(1)

    contParametros += 1

#Sintaxis para exprecionesArismeticas
def p_exprecionArismetica(p):
    '''
    exprecionArismetica : asignarMultDiv cuadruploSR SignoSumRest addOperador exprecionArismetica
                        | asignarMultDiv cuadruploSR            
    '''
def p_asignarMultDiv(p):
    '''
    asignarMultDiv      : asignarValorArismetico cuadruploMD SignoMultDiv addOperador asignarMultDiv
                        | asignarValorArismetico cuadruploMD
    '''
def p_asignarValorArismetico(p):
    '''
    asignarValorArismetico  : valor addListaVariables
                            | ParentecisIzq addLista estatuto cerrarLista ParentecisDer
                            | ParentecisIzq addLista exprecionArismetica cerrarLista ParentecisDer
    '''

#Funciones para listas de funciones arismeticas

#Saca el ultimo valor resultado de la lista de variables (utilizar despues de una exprecion)
def p_valorFinal(p):
    "valorFinal :"
    valorFinal = listaVO[str(contListasVO)]['variables'].pop()
    p[0] = valorFinal

#Agrega la ultima variable en la lista de variables
def p_addListaVariables(p):
    "addListaVariables :"
    p_addListaDir(p[-1])
    listaVO[str(contListasVO)]['variables'].append(p[-1])

#Guarda las direcciones que se han declarado (para saber cual fue la ultima direccion creada)
def p_addListaDir(var):
    global listaDir

    func = validarVar(var)

    if tablaVariables[func]['variables'][var]['limites'] != None:
        listaDir.append(tablaVariables[func]['variables'][var]['direccion'])

#Agregar el operador en la lista de operadores
def p_addOperador(p):
    "addOperador :"
    listaVO[str(contListasVO)]['operador'].append(p[-1])
            
#Agrega uno a contListasVo, el cual permite realizar expreciones arismeticas mas especificas
def p_addLista(p):
    "addLista :"
    global contListasVO

    contListasVO += 1
    listaVO[str(contListasVO)] = {'operador': [], 'variables': []}

#Se usa para cerrar los parentecis en las expreciones arismeticas (regresa un valor, casi siempre temporal)
def p_cerrarLista(p):
    "cerrarLista :"
    global contListasVO

    if contListasVO > 0:
        #regresa el valor de la lista especial a la actual
        listaVO[str(contListasVO - 1)]['variables'].append(listaVO[str(contListasVO)]['variables'].pop())
        del listaVO[str(contListasVO)]
        contListasVO -= 1


def p_valor(p):
    '''
    valor               : ConstanteInt validarInt
                        | ConstanteFloat validarFloat
                        | tipoIdentificador validarVariable
                        | llamarFuncionAsignacion validarLlamada
    '''
    if p[1] != None:
        p[0] = p[1]
    else:
        p[0] = p[2]
def p_lectura(p):
    '''
    lectura             : Lee ParentecisIzq valoresLectura ParentecisDer PuntoComa
    '''
def p_valoresLectura(p):
    '''
    valoresLectura      : tipoIdentificador cuadruploLectura
                        | tipoIdentificador cuadruploLectura Coma valoresLectura
    '''

    p[0] = p[1]

#crea el cuadruplo lee
def p_cuadruploLectura(p):
    "cuadruploLectura :"
    global contCuadruplos

    #encuentra la memoria
    func = validarVar(p[-1])
    mem = encontrarMem(func,p[-1])

    #crea el cuadruplo
    cuadruplos.append('Lee ' + str(mem))
    contCuadruplos += 1


def p_escritura(p):
    '''
    escritura           : Escribe ParentecisIzq definirEscritura ParentecisDer PuntoComa
    '''
def p_definirEscritura(p):
    '''
    definirEscritura    : asignarValorEscritura cuadruploEscritura
                        | asignarValorEscritura cuadruploEscritura Coma definirEscritura
    '''

def p_asignarValorEscritura(p):
    '''
    asignarValorEscritura   : exprecionArismetica valorFinal
                            | tipoIdentificador validarVariable
                            | llamarFuncionAsignacion validarLlamada
                            | ConstanteChar validarChar
    '''
    if p[1] != None:
        p[0] = p[1]
    else:
        p[0] = p[2]

#Crea el cuadruplo Escr
def p_cuadruploEscritura(p):
    "cuadruploEscritura :"
    global contCuadruplos
    
    #Encuentra la memoria de la variable
    func = validarVar(p[-1])
    mem = encontrarMem(func,p[-1])

    #crea el cuadruplo
    cuadruplos.append('Escr ' + str(mem))
    contCuadruplos += 1

def p_si(p):
    '''
    si                  : Si ParentecisIzq exprecionLogica ParentecisDer gotoF Entonces BraquetIzq estatuto BraquetDer sino
    '''

def p_exprecionLogica(p):
    '''
    exprecionLogica     : asignarOperadorLogico cuadruploSignoLogico SignoLogico addOperadorLogico exprecionLogica
                        | asignarOperadorLogico cuadruploSignoLogico
    '''

def p_asignarOperadorLogico(p):
    '''
    asignarOperadorLogico   : asignarValor addVariableLogica cuadruploOperadorLogico OperadorLogico addOperadorLogico asignarOperadorLogico
                            | asignarValor addVariableLogica cuadruploOperadorLogico 
    '''
def p_addSignoLogico(p):
    "addVariableLogica :"
    
    
    func = validarVar(p[-1])

    if tablaVariables[func]['variables'][p[-1]]['limites'] != None:
        listaDir.append(tablaVariables[func]['variables'][p[-1]]['direccion'])
    

    listaVarLogicos.append(p[-1])


#Agregar operador logico a la lista
def p_addOperadorLogico(p):
    "addOperadorLogico :"
    listaOpeLogicos.append(p[-1])

def p_sino(p):
    '''
    sino                : popGotoFs Sino goto BraquetIzq estatuto BraquetDer popGoto
                        | popGotoF
    '''

def p_mientras(p):
    '''
    mientras            : Mientras ParentecisIzq gotoM exprecionLogica ParentecisDer gotoF Haz BraquetIzq estatuto BraquetDer popGotoFs popGotoM
    '''
def p_desde(p):
    '''
    desde               : Desde tipoIdentificador validarIdDesde Igual asignarValor asignarDesde Hasta gotoM asignarValor comparaDesde gotoF Hacer BraquetIzq estatuto BraquetDer addUnoVariable popGotoFs popGotoM
    '''

#validar variable a utilizar (tiene que estar en la funcion actual)
def p_validarIdDesde(p):
    "validarIdDesde :"

    if p[-1] not in tablaVariables[funcionActual]['variables']:
        print(p[-1])
        print("Error, Variable no identificada")
        sys.exit(1)

#agrega el cuadruplo de asignacion a la variabe que va a inicializar el ciclo de ejecucion
def p_asignarDesde(p):
    "asignarDesde :"

    global contCuadruplos
    
    #encontrar tipo y memoria de variable
    funcId = validarVar(p[-4])
    memId = encontrarMem(funcId,p[-4])
    tipoId = tablaVariables[funcId]['variables'][p[-4]]['tipoVariable']

    #encontrar tipo y memoria de resultado de exprecion
    funcValor = validarVar(p[-1])
    memValor = encontrarMem(funcValor,p[-1])
    tipoValor = tablaVariables[funcValor]['variables'][p[-1]]['tipoVariable']

    #validar que pueda ser compatible 
    tipo = designer('=',tipoId,tipoValor)
    if tipo == 'err':
        print('ERROR, Variables incompatibles')
        sys.exit(1)
    
    #crear cuadruplo
    cuadruplos.append('= ' + str(memValor) + ' ' + str(memId))
    contCuadruplos += 1

#compara la variable asignada con la exprecion 
def p_comparaDesde(p):
    "comparaDesde :"
    global contCuadruplos
    global contVarTemp

    #encontrar tipo y memoria de variable
    funcId = validarVar(p[-8])
    memId = encontrarMem(funcId,p[-8])
    tipoId = tablaVariables[funcId]['variables'][p[-8]]['tipoVariable']

    #encontrar tipo y memoria de resultado de exprecion
    funcValor = validarVar(p[-1])
    memValor = encontrarMem(funcValor,p[-1])
    tipoValor = tablaVariables[funcValor]['variables'][p[-1]]['tipoVariable']

    #validar que pueda ser compatible y crear variable temporal
    tipo = designer('<=',tipoId,tipoValor)
    temp = addVarTemp(tipo)
    memoria_temp = tablaVariables[funcionActual]['variables'][temp]['memoria']

    #crear cuadruplo
    cuadruplos.append('<= ' + str(memId) + ' ' + str(memValor) + ' ' + str(memoria_temp))
    contCuadruplos += 1
    contVarTemp += 1

#Sumarle uno a la variable asignada del desde
def p_addUnoVariable(p):
    "addUnoVariable :"
    global contCuadruplos
    global contVarTemp

    #agregar variable 1
    addConst(1)

    #encontrar funcion de variable asignada
    funcId = validarVar(p[-14])
    memId = encontrarMem(funcId,p[-14])
    tipoId = tablaVariables[funcId]['variables'][p[-14]]['tipoVariable']

    #validar que sea tipo int
    if tipoId != 'int':
        print("ERROR, Variable dentro del desde diferente a int")
        sys.exit(1)

    #encontrar memoria de tipo int
    funcValor = validarVar(1)
    memValor = encontrarMem(funcValor,1)
    tipoValor = tablaVariables[funcValor]['variables'][1]['tipoVariable']

    #validar que se pueda sumar sin error
    tipo = designer('+',tipoId,tipoValor)
    #crear memoria temporal
    temp = addVarTemp(tipo)
    memoria_temp = tablaVariables[funcionActual]['variables'][temp]['memoria']

    #crear cuadruplo para sumar uno y poner en variable temporal
    cuadruplos.append('+ ' + str(memId) + ' ' + str(memValor) + ' ' + str(memoria_temp))
    contCuadruplos += 1
    contVarTemp += 1

    tipoTemp = tablaVariables[funcionActual]['variables'][temp]['tipoVariable']

    #validar que se pueda igualar
    comp = designer('=',tipoId,tipoTemp)

    if comp == 'err':
        print("ERROR, error al sumar 1 en Desde")
        sys.exit(1)

    #crear cuadruplo para asignar variable temporal a variable asignada
    cuadruplos.append('= ' + str(memoria_temp) + ' ' + str(memId))
    contCuadruplos += 1





def p_regresa(p):
    '''
    regresa             : Regresa ParentecisIzq asignarValor regresaCuadruplo ParentecisDer PuntoComa
    '''

    p[0] = 'Regresa'

#regresar el valor de funcion
def p_regresaCuadruplo(p):
    "regresaCuadruplo   :"
    global contCuadruplos

    
    #validar si regresa variable local o global
    if p[-1] in tablaVariables[funcionActual]['variables']:
        #validar si es un arreglo o no
        if tablaVariables[funcionActual]['variables'][p[-1]]['limites'] != None:
            memoriaVar = tablaVariables[funcionActual]['variables'][p[-1]]['direccion']
        else:
            memoriaVar = tablaVariables[funcionActual]['variables'][p[-1]]['memoria']
    elif p[-1] in tablaVariables['global']['variables']:
        #validar si es un arreglo o no
        if tablaVariables['global']['variables'][p[-1]]['limites'] != None:
            memoriaVar = tablaVariables['global']['variables'][p[-1]]['direccion']
        else:
            memoriaVar = tablaVariables['global']['variables'][p[-1]]['memoria']
    else:
        print(p[-1])
        print('ERROR, variable no identificada')
        sys.exit(1)

    cuadruplos.append('regresa ' + str(memoriaVar))
    contCuadruplos += 1

#--------------------------------------------------------
#Guardar variables 


#Funcion que detecta el tipo y nombre de la variable,
#Valida si no existe y la da de alta en la tabla de variables de la funcuin actual
def p_defineTipo(p):
    "defineTipo :"
    global banderVariable
    global tipoVariable
    global limiteArray

    #Checa si la variable esta por coma o no (para saber que tan lejano esta el tipo de la variable)
    #Encuentra el tipo y nombre de la variable
    if banderVariable:
        if p[-1] == ',':
            tipoVariable = p[-3]
            banderVariable = False
            nombreVal = p[-2]
        else:
            tipoVariable = p[-2]
            nombreVal = p[-1]
    else:
        if p[-1] == ',':
            nombreVal = p[-2]
        else:
            nombreVal = p[-1]

    #Valida que la variable a declarar no este previamente declarada
    if nombreVal not in tablaVariables[funcionActual]['variables'] and nombreVal not in tablaVariables:

        num = validarLocalVar(tipoVariable)

        #Checa si es un arreglo o no
        if limiteArray == None:
            #En caso de no ser arreglo, da de alta la variable en la lista de variable y asigna memoria
            tablaVariables[funcionActual]['variables'][nombreVal] = {'nombreVariable':nombreVal,'tipoVariable':tipoVariable,'memoria': num, 'limites':None, 'direccion':None}
        else:
            #Valida que la constante del limite del arreglo tenga un lugar en la memoria
            addLimiteArray(limiteArray)
            #Checa que el limite del arreglo sea de tipo int
            if tablaVariables[funcionActual]['variables'][limiteArray]['tipoVariable'] == 'int':
                #valida y agrega la constante 0
                addConst(0)    
                #Agrega la nueva variable a la table de variables y le asigna su memoria
                tablaVariables[funcionActual]['variables'][nombreVal] = {'nombreVariable':nombreVal,'tipoVariable':tipoVariable,'memoria': num, 'limites':limiteArray, 'direccion':None}
            else:
                print('ERROR, Dimencion de arreglo invalida')
                sys.exit(1)
    else:
        print("Error, variable no valida")
        sys.exit(1)
    limiteArray = None

#Valida que la constante del limite del arreglo tenga un lugar en la memoria
def addLimiteArray(limiteArray):
    #Valida si el numero existe
    if limiteArray not in tablaVariables[funcionActual]['variables']:
        if str(limiteArray) in varConst:
            num = varConst[str(limiteArray)]['memoria']
        else:
            num = memConst()

        #En caso de no tener crea la variable y le asigna su espacio de memoria
        tablaVariables[funcionActual]['variables'][limiteArray] = {'nombreVariable':limiteArray,'tipoVariable':'int','memoria': num, 'limites':None, 'direccion':None} 


#--------------------------------------------------------
#EliminarVariablesLocales
def p_popVars(p):
    "popVars :"
    global contMemFuncion

    #Iterador para todas las variables de la funcion actual
    for i in tablaVariables[funcionActual]['variables']:

        #Asigna la memoria de las variables en memoriaFunciones(Lista que se va a utilizar para las variables globales)
        mem = tablaVariables[funcionActual]['variables'][i]['memoria']
        memoriaFunciones[funcionActual][mem] = contMemFuncion
        contMemFuncion += 1

        #Checa si es un arreglo o no
        if tablaVariables[funcionActual]['variables'][i]['limites'] == None:
            #Si no es arreglo, valida que no sea constante (ya que esas no repiten el espacio de memoria)
            if tablaVariables[funcionActual]['variables'][i]['memoria'] < 6000 or tablaVariables[funcionActual]['variables'][i]['memoria'] >= 8999:
                memoriaDisponible[tablaVariables[funcionActual]['variables'][i]['memoria']] = tablaVariables[funcionActual]['variables'][i]['memoria']
        else:
            #En caso de que sea arreglo, borra todos los espacios de memoria dependientes a esta variable
            inicio = tablaVariables[funcionActual]['variables'][i]['memoria']
            limite = tablaVariables[funcionActual]['variables'][i]['limites']
            for j in range(limite):
                memoriaDisponible[j + inicio] = j + inicio
    
    #Limpia la tabla de variables actual
    tablaVariables[funcionActual]['variables'].clear()

#--------------------------------------------------------
#EndFunc

#Agrega el cuadruplo que indica el final de la funcion 
def p_addEndFunc(p):
    "addEndFunc :"
    global contCuadruplos

    cuadruplos.append('endFunc')
    contCuadruplos +=1

#Crea el cuadruplo endProgram
def p_addEndProgram(p):
    "addEndProgram :"

    global contCuadruplos

    cuadruplos.append('endProgram')
    contCuadruplos +=1


#--------------------------------------------------------
#GOTO, GOTOF, GOTOV

#Goto

#Se crea el cuadruplo Goto y se guarda en listaGoto el cuadruplo actual 
#para sacarlo cuando sea necesario
def p_goto(p):
    "goto :"
    global contCuadruplos

    cuadruplos.append('Goto ')
    listaGoto.append(contCuadruplos)
    contCuadruplos +=1

#Asigna saca el ultimo valor de la listaGoto para saber el cuandruplo
#en el que estaba y le asigna el cuadruplo actual
def p_popGoto(p):
    "popGoto :"
    global contCuadruplos

    cuadruplos[listaGoto.pop()] += str(contCuadruplos)

#Agregar a la lista el cuadruplo actual
def p_gotoM(p):
    "gotoM :"
    global contCuadruplos

    listaGoto.append(contCuadruplos)

#Agrega el cuadruplo Goto 
def p_popGotoM(p):
    "popGotoM :"
    global contCuadruplos

    #crear cuadriplo goto 
    cuadruplos.append('Goto ' + str(listaGoto.pop()))
    contCuadruplos +=1

#GotoF

#crea cuadruplo GotoF
def p_gotoF(p):
    "gotoF :"
    global contCuadruplos
    global contVarTemp

    #Tiene que existir una variable temporal (ahi se guarda su es True o False)
    if varTemp == []:
        print("ERROR, Exprecion logica invalida")
        sys.exit(1)
    #Crear cuadrupolo con el ultimo temporal
    cuadruplos.append('GotoF ' + str(varTemp[contVarTemp-1]['memoria']) + ' ')
    listaGoto.append(contCuadruplos)
    contCuadruplos += 1

#Se llama la lista Goto que guarda todos los cuadruplos que tienen goto pendientes
def p_popGotoFs(p):
    "popGotoFs :"
    global contCuadruplos

    #Se le agrega uno en los casos que tengan Goto al final (por que falta por agregar a este punto)
    cuadruplos[listaGoto.pop()] += str(contCuadruplos + 1)

#Se llama la lista Goto que guarda todos los cuadruplos que tienen goto pendientes
def p_popGotoF(p):
    "popGotoF :"
    global contCuadruplos

    #No se le agrega nada por que no hay un goto despues
    cuadruplos[listaGoto.pop()] += str(contCuadruplos)


#------------------------------------------------------

#------------------------------------------------------
#ADD CUADRUPLOS

#Crear los cuadruplos de expreciones arismeticas
def p_cuadruploMD(p):
    "cuadruploMD :"
    global varTemp
    global contVarTemp
    global contCuadruplos
    
    #Existe algo en la lista de operadores
    if listaVO[str(contListasVO)]['operador'] != []:
        ultimoOperador = len(listaVO[str(contListasVO)]['operador'])-1
        #la ultima variable de la lista de operadores es '*' o '/'
        if listaVO[str(contListasVO)]['operador'][ultimoOperador] == '*' or listaVO[str(contListasVO)]['operador'][ultimoOperador] == '/':
            #sacar el operador
            operador = listaVO[str(contListasVO)]['operador'].pop()
            #sacar la ultima variable (va a ser la derecha por que es de izq a derecha)
            operando_Der = listaVO[str(contListasVO)]['variables'].pop()
            #sacar la ultima variable (va a ser la izquierda)
            operando_Izq = listaVO[str(contListasVO)]['variables'].pop()
            
            #encontrar en que funciones estan estas variables (global o local)
            funcDer = validarVar(operando_Der)
            funcIzq = validarVar(operando_Izq)

            encontrarMem(funcDer,operando_Der)

            #enconrar el tipo de cada funcion y saber si son compatibles en la operacion que se esta haceiendo
            tipo_Der = tablaVariables[funcDer]['variables'][operando_Der]['tipoVariable']
            tipo_Izq = tablaVariables[funcIzq]['variables'][operando_Izq]['tipoVariable']
            tipo = designer(operador,tipo_Izq,tipo_Der)

            #crear variable temporal
            temp = addVarTemp(tipo)

            #encontrar las memorias y en caso de ser direcciones la memoria de la direccion
            memoria_Der = encontrarMem(funcDer,operando_Der)
            memoria_Der = encontrarDir(memoria_Der)
            memoria_Izq = encontrarMem(funcIzq,operando_Izq)
            memoria_Izq = encontrarDir(memoria_Izq)
            memoria_temp = tablaVariables[funcionActual]['variables'][temp]['memoria']

            #Agregar la variable temporal (que va a guardar el valor resultante de la operacion) en la lista listaVO
            listaVO[str(contListasVO)]['variables'].append(temp)

            #Crear cuadruplo
            cuadruplos.append(str(operador) + ' ' + str(memoria_Izq) + ' ' + str(memoria_Der) + ' ' + str(memoria_temp))
            contCuadruplos += 1
            contVarTemp += 1

#Crear los cuadruplos de expreciones arismeticas despues de validar si existen multiplicaciones o diviciones pendientes
def p_cuadruploSR(p):
    "cuadruploSR :"
    global varTemp
    global contVarTemp
    global contCuadruplos

    #Existe algo en la lista de operadores
    if listaVO[str(contListasVO)]['operador'] != []:
        ultimoOperador = len(listaVO[str(contListasVO)]['operador'])-1
        #la ultima variable de la lista de operadores es '+' o '-'
        if listaVO[str(contListasVO)]['operador'][ultimoOperador] == '+' or listaVO[str(contListasVO)]['operador'][ultimoOperador] == '-':
            #sacar el operador
            operador = listaVO[str(contListasVO)]['operador'].pop()
            #sacar la ultima variable (va a ser la derecha por que es de izq a derecha)
            operando_Der = listaVO[str(contListasVO)]['variables'].pop()
            #sacar la ultima variable (va a ser la izquierda)
            operando_Izq = listaVO[str(contListasVO)]['variables'].pop()

            #encontrar en que funciones estan estas variables (global o local)
            funcDer = validarVar(operando_Der)
            funcIzq = validarVar(operando_Izq)

            #enconrar el tipo de cada funcion y saber si son compatibles en la operacion que se esta haceiendo
            tipo_Der = tablaVariables[funcDer]['variables'][operando_Der]['tipoVariable']
            tipo_Izq = tablaVariables[funcIzq]['variables'][operando_Izq]['tipoVariable']
            tipo = designer(operador,tipo_Izq,tipo_Der)

            #crear variable temporal
            temp = addVarTemp(tipo)
            
            #encontrar las memorias y en caso de ser direcciones la memoria de la direccion
            memoria_Der = encontrarMem(funcDer,operando_Der)
            memoria_Der = encontrarDir(memoria_Der)
            memoria_Izq = encontrarMem(funcIzq,operando_Izq)
            memoria_Izq = encontrarDir(memoria_Izq)
            memoria_temp = tablaVariables[funcionActual]['variables'][temp]['memoria']

            #Agregar la variable temporal (que va a guardar el valor resultante de la operacion) en la lista listaVO
            listaVO[str(contListasVO)]['variables'].append(varTemp[contVarTemp])
            listaVO[str(contListasVO)]['variables'].append(temp)

            #Crear cuadruplo
            cuadruplos.append(str(operador) + ' ' + str(memoria_Izq) + ' ' + str(memoria_Der) + ' ' + str(memoria_temp))
            contCuadruplos += 1
            contVarTemp += 1

#Crea los cuadruplos de exprecions logicas despues de validas que ya se hayan realizando los operadores logicos
def p_cuadruploSignoLogico(p):
    "cuadruploSignoLogico :"
    global varTemp
    global contVarTemp
    global contCuadruplos

    #Existe algo en la lista de operadores
    if listaOpeLogicos != []:
        #la ultima variable de la lista de operadores es '&' o '|'
        if listaOpeLogicos[len(listaOpeLogicos)-1] == '&' or listaOpeLogicos[len(listaOpeLogicos)-1] == '|':
            #sacar el operador
            operador = listaOpeLogicos.pop()
            #sacar la ultima variable (va a ser la derecha por que es de izq a derecha)
            operando_Der = listaVarLogicos.pop()
            #sacar la ultima variable (va a ser la izquierda)
            operando_Izq = listaVarLogicos.pop()

            #encontrar en que funciones estan estas variables (global o local)
            funcDer = validarVar(operando_Der)
            funcIzq = validarVar(operando_Izq)

            #enconrar el tipo de cada funcion y saber si son compatibles en la operacion que se esta haceiendo
            tipo_Der = tablaVariables[funcDer]['variables'][operando_Der]['tipoVariable']
            tipo_Izq = tablaVariables[funcIzq]['variables'][operando_Izq]['tipoVariable']
            tipo = designer(operador,tipo_Izq,tipo_Der)

            #crear variable temporal
            temp = addVarTemp(tipo)

            #encontrar las memorias y en caso de ser direcciones la memoria de la direccion
            memoria_Der = encontrarMem(funcDer,operando_Der)
            memoria_Der = encontrarDir(memoria_Der)
            memoria_Izq = encontrarMem(funcIzq,operando_Izq)
            memoria_Izq = encontrarDir(memoria_Izq)
            memoria_temp = tablaVariables[funcionActual]['variables'][temp]['memoria']

            #Agregar la variable temporal (que va a guardar el valor resultante de la operacion) en la lista listaVarLogicos
            listaVarLogicos.append(varTemp[contVarTemp]['nombreVariable'])
            #Crear cuadruplo
            cuadruplos.append(str(operador) + ' ' + str(memoria_Izq) + ' ' + str(memoria_Der) + ' ' + str(memoria_temp))

            contCuadruplos += 1
            contVarTemp += 1

#Crea los cuadruplos de exprecions logicas despues de validas que ya se hayan realizando los operadores logicos
def p_cuadruploOperadorLogico(p):
    "cuadruploOperadorLogico :"
    global varTemp
    global contVarTemp
    global contCuadruplos

    #Existe algo en la lista de operadores
    if listaOpeLogicos != []:
        #la ultima variable de la lista de operadores es '==' o '>=' o '<=' o '<' o '>'
        if listaOpeLogicos[len(listaOpeLogicos)-1] == '==' or listaOpeLogicos[len(listaOpeLogicos)-1] == '>=' or listaOpeLogicos[len(listaOpeLogicos)-1] == '<=' or listaOpeLogicos[len(listaOpeLogicos)-1] == '>' or listaOpeLogicos[len(listaOpeLogicos)-1] == '<':
            #sacar el operador
            operador = listaOpeLogicos.pop()
            #sacar la ultima variable (va a ser la derecha por que es de izq a derecha)
            operando_Der = listaVarLogicos.pop()
            #sacar la ultima variable (va a ser la izquierda)
            operando_Izq = listaVarLogicos.pop()

            #encontrar en que funciones estan estas variables (global o local)
            funcDer = validarVar(operando_Der)
            funcIzq = validarVar(operando_Izq)

            #enconrar el tipo de cada funcion y saber si son compatibles en la operacion que se esta haceiendo
            tipo_Der = tablaVariables[funcDer]['variables'][operando_Der]['tipoVariable']
            tipo_Izq = tablaVariables[funcIzq]['variables'][operando_Izq]['tipoVariable']
            tipo = designer(operador,tipo_Izq,tipo_Der)

            #crear variable temporal
            temp = addVarTemp(tipo)

            #encontrar las memorias y en caso de ser direcciones la memoria de la direccion
            memoria_Der = encontrarMem(funcDer,operando_Der)
            memoria_Der = encontrarDir(memoria_Der)
            memoria_Izq = encontrarMem(funcIzq,operando_Izq)
            memoria_Izq = encontrarDir(memoria_Izq)
            memoria_temp = tablaVariables[funcionActual]['variables'][temp]['memoria']

            #Agregar la variable temporal (que va a guardar el valor resultante de la operacion) en la lista listaVarLogicos
            listaVarLogicos.append(varTemp[contVarTemp]['nombreVariable'])
            #Crear cuadruplo
            cuadruplos.append(str(operador) + ' ' + str(memoria_Izq) + ' ' + str(memoria_Der) + ' ' + str(memoria_temp))
            
            contCuadruplos += 1
            contVarTemp += 1

#Encuentra la memoria de las variables asignadas
def encontrarMem(func,nomVar):

    #checa si es arreglo
    if tablaVariables[func]['variables'][nomVar]['limites'] == None:
        #regresa el espacio de memoria
        return tablaVariables[func]['variables'][nomVar]['memoria']
    else:
        #regresa la memoria a la que esta apuntando
        return tablaVariables[func]['variables'][nomVar]['direccion']

#Encuentra la ultima variable direccion que se creo
def encontrarDir(num):
    if num >= 12000:
        return listaDir.pop()
    return num
#------------------------------------------------------
#ValidarVariablesLocales

#Encuentra el espacio de memoria disponible para las variables que se estan declarando
def validarLocalVar(tipoVar):
    global limiteArray

    #Valida si es arreglo
    if limiteArray == None:

        if funcionActual == 'global':
            #Encuentra espacio de memoria para variables globales
            return memGlobal(tipoVar)
        else:
            #Encuentra espacio de memoria para variables Locales
            return memLocal(tipoVar)  
    else:
        if funcionActual == 'global':
            #Encuentra la seccion de espacio de memoria del arreglo en las Globales
            return memGlobalArray(tipoVar,limiteArray)
        else:
            #Encuentra la seccion de espacio de memoria del arreglo en las Locales
            return memLocalArray(tipoVar, limiteArray)

#------------------------------------------------------------
#EncontrarMemoria

#Encuentra memoria disponible en la seccion de globales
def memGlobal(tipoVar):
    num = None
    #Checa en todos los espacios disponibles
    for i in range(999):
        if i == 1000:
            break
        
        #Valida si es int
        if tipoVar == 'int':
            #Ya encontro el espacio de memoria
            if memoriaDisponible[i] != None:
                #guarda la memoria, pone ocupado el espacio de memoria y termina de iterar
                num = i
                memoriaDisponible[i] = None
                break
        #Valida si es float
        elif tipoVar == 'float':
            #Ya encontro el espacio de memoria
            if memoriaDisponible[i + 1000] != None:
                #guarda la memoria, pone ocupado el espacio de memoria y termina de iterar
                num = i + 1000
                memoriaDisponible[i + 1000] = None
                break
        #Valida si es char
        elif tipoVar == 'char':
            #Ya encontro el espacio de memoria
            if memoriaDisponible[i + 2000] != None:
                #guarda la memoria, pone ocupado el espacio de memoria y termina de iterar
                num = i + 2000
                memoriaDisponible[i + 2000] = None
                break
        else:
            print("Error, Tipo de variable no reconocido GLOBAL")
            sys.exit(1)
    if num == None:
        print("Error, No hay espacio de memoria para la variabe")
        sys.exit(1)
    
    #Regresa la memoria encontrada
    return num



def memLocal(tipoVar):
    num = None
    #Checa en todos los espacios disponibles
    for i in range(999):
        if i == 1000:
            break

        #Valida si es int
        if tipoVar == 'int':
            #Ya encontro el espacio de memoria
            if memoriaDisponible[i + 3000] != None:
                #guarda la memoria, pone ocupado el espacio de memoria y termina de iterar
                num = i + 3000
                memoriaDisponible[i + 3000] = None
                break
        #Valida si es float
        elif tipoVar == 'float':
            #Ya encontro el espacio de memoria
            if memoriaDisponible[i + 4000] != None:
                #guarda la memoria, pone ocupado el espacio de memoria y termina de iterar
                num = i + 4000
                memoriaDisponible[i + 4000] = None
                break
        #Valida si es char
        elif tipoVar == 'char':
            #Ya encontro el espacio de memoria
            if memoriaDisponible[i + 5000] != None:
                #guarda la memoria, pone ocupado el espacio de memoria y termina de iterar
                num = i + 5000
                memoriaDisponible[i + 5000] = None
                break
        else:
            print("Error, Tipo de variable no reconocido LOCAL")
            sys.exit(1)
    if num == None:
        print("Error, No hay espacio de memoria para la variabe")
        sys.exit(1)
    #Regresa la memoria encontrada
    return num


def memGlobalArray(tipoVar, limiteArray):
    espacio = True

    num = None
    tipoMem = 0
    #Checa en todos los espacios disponibles
    for i in range(1000):
        
        #Checa si hay memoria suficiente para todos los espacios del arreglo (tienen que estar todos juntos)
        for j in range(limiteArray):
            if i + j >= 1000:
                espacio = False
                break
            #Valida si es int
            if tipoVar == 'int':
                tipoMem = 0
                #Si no encuentra espacio rompe el for (tiene qu eestar toda la memoria junta)
                if memoriaDisponible[i + j] == None:
                    espacio = False
                    break
            #Valida si es float
            elif tipoVar == 'float':
                #Si no encuentra espacio rompe el for (tiene qu eestar toda la memoria junta)
                if memoriaDisponible[i + j + 1000] == None:
                    tipoMem = 1000
                    espacio = False
                    break
            #Valida si es char
            elif tipoVar == 'char':
                tipoMem = 2000
                #Si no encuentra espacio rompe el for (tiene qu eestar toda la memoria junta)
                if memoriaDisponible[i + j + 2000] == None:
                    espacio = False
                    break
            else:
                print("Error, Tipo de variable no reconocido GLOBAL ARR")
                sys.exit(1)
            espacio = True
        
        #Si espacio termina en True, significa que si se encontro memoria, en caso de que no, sigue iterando la lista
        if espacio == True:
            num = i + tipoMem
            for j in range(limiteArray):
                #Marca todos los espacios de meoria del arreglo como ocupados
                memoriaDisponible[num+j] = None
            return num 

    print("Error, No hay espacio de memoria para el arreglo")
    sys.exit(1)

#Encuentra espacio de memoria para los arreglos locales
def memLocalArray(tipoVar, limiteArray):
    espacio = True

    num = None
    tipoMem = 0
    #Checa en todos los espacios disponibles
    for i in range(1000):
        
        #Checa si hay memoria suficiente para todos los espacios del arreglo (tienen que estar todos juntos)
        for j in range(limiteArray):
            if i + j >= 1000:
                espacio = False
                break
            #Valida si es int
            if tipoVar == 'int':
                tipoMem = 3000
                #Si no encuentra espacio rompe el for (tiene qu eestar toda la memoria junta)
                if memoriaDisponible[i + j + 3000] == None:
                    espacio = False
                    break
            #Valida si es float
            elif tipoVar == 'float':
                #Si no encuentra espacio rompe el for (tiene qu eestar toda la memoria junta)
                if memoriaDisponible[i + j + 4000] == None:
                    tipoMem = 4000
                    espacio = False
                    break
            #Valida si es char
            elif tipoVar == 'char':
                tipoMem = 5000
                #Si no encuentra espacio rompe el for (tiene qu eestar toda la memoria junta)
                if memoriaDisponible[i + j + 5000] == None:
                    espacio = False
                    break
            else:
                print("Error, Tipo de variable no reconocido LOCAL ARR")
                sys.exit(1)
            espacio = True
        
        #Si espacio termina en True, significa que si se encontro memoria, en caso de que no, sigue iterando la lista
        if espacio == True:
            num = i + tipoMem
            #Marca todos los espacios de meoria del arreglo como ocupados
            for j in range(limiteArray):
                memoriaDisponible[num+j] = None
            return num 

    print("Error, No hay espacio de memoria para el arreglo")
    sys.exit(1)

#Encuentra memoria para constantes
def memConst():
    num = None
    #Checa en todos los espacios disponibles
    for i in range(3000):
        if i == 3000:
            break
        #Si encuentra espacio de memoria disponible lo asigna
        if memoriaDisponible[i + 6000] != None:
            num = i + 6000
            memoriaDisponible[i + 6000] = None
            break
    
    if num == None:
        print("Error, No hay espacio de memoria para la constante")
        sys.exit(1)

    return num

#Encuentra memoria para temporales
def memTemporal():
    num = None
    #Checa en todos los espacios disponibles
    for i in range(3000):
        if i == 3000:
            break
        #Si encuentra espacio de memoria disponible lo asigna
        if memoriaDisponible[i + 9000] != None:
            num = i + 9000
            memoriaDisponible[i + 9000] = None
            break
    
    if num == None:
        print("Error, No hay espacio de memoria para la variabe temporal")
        sys.exit(1)

    return num

#Encuentra memoria para apuntadores
def memDir():
    num = None
    #Checa en todos los espacios disponibles
    for i in range(2000):
        if i == 2000:
            break
        #Si encuentra espacio de memoria disponible lo asigna
        if memoriaDisponible[i + 12000] != None:
            num = i + 12000
            memoriaDisponible[i + 12000] = None
            break
    
    if num == None:
        print("Error, No hay espacio de memoria para la variabe temporal")
        sys.exit(1)

    return num




#Agregar const

def addConst(const):
    #valida si la variable ya estaba declarada previamente
    if const not in tablaVariables['global']['variables']:
        if const not in tablaVariables[funcionActual]['variables']:
            
            #Si no se encontro se da de alta
            #Se busca en todas las constantes declaradas anteriores para no duplicar memoria
            if str(const) in varConst:
                #Se asigna la memoria previa
                num = varConst[str(const)]['memoria']
                tablaVariables[funcionActual]['variables'][const] = {'nombreVariable':const,'tipoVariable':'int','memoria': num, 'limites':None, 'direccion':None}
            else:
                #Se busca una nueva memoria
                num = memConst()

                #Se da de alta la variable en la funcion actual
                tablaVariables[funcionActual]['variables'][const] = {'nombreVariable':const,'tipoVariable':'int','memoria': num, 'limites':None, 'direccion':None} 
                #Se guarda la nueva varruable y su memoria en la lista varConst
                varConst[str(const)] = {'nombre': const, 'memoria':num}

    #si la variable no se encontro en las variables locales pero si en las globales, se agrega en las variables locales (se ocupa para la maquina virtual)
    if funcionActual != 'global' and const not in tablaVariables[funcionActual]['variables']:
        if str(const) in varConst:
            num = varConst[str(const)]['memoria']
        else:
            num = memConst()

            tablaVariables[funcionActual]['variables'][const] = {'nombreVariable':const,'tipoVariable':'int','memoria': num, 'limites':None, 'direccion':None} 
            varConst[str(const)] = {'nombre': const, 'memoria':num}

#ValidarConstantes

def p_validarInt(p):
    "validarInt :"
    
    #valida si la variable ya estaba declarada previamente
    if p[-1] not in tablaVariables['global']['variables']:
        if p[-1] not in tablaVariables[funcionActual]['variables']:

            #Si no se encontro se da de alta
            #Se busca en todas las constantes declaradas anteriores para no duplicar memoria
            if str(p[-1]) in varConst:
                #Se asigna la memoria previa
                num = varConst[str(p[-1])]['memoria']
                tablaVariables[funcionActual]['variables'][p[-1]] = {'nombreVariable':p[-1],'tipoVariable':'int','memoria': num, 'limites':None, 'direccion':None}
            else:
                #Se busca una nueva memoria
                num = memConst()

                #Se da de alta la variable en la funcion actual
                tablaVariables[funcionActual]['variables'][p[-1]] = {'nombreVariable':p[-1],'tipoVariable':'int','memoria': num, 'limites':None, 'direccion':None}   
                #Se guarda la nueva varruable y su memoria en la lista varConst
                varConst[str(p[-1])] = {'nombre': p[-1], 'memoria':num}

    #si la variable no se encontro en las variables locales pero si en las globales, se agrega en las variables locales (se ocupa para la maquina virtual)
    if funcionActual != 'global' and p[-1] not in tablaVariables[funcionActual]['variables']:
        if str(p[-1]) in varConst:
            num = varConst[str(p[-1])]['memoria']
            tablaVariables[funcionActual]['variables'][p[-1]] = {'nombreVariable':p[-1],'tipoVariable':'int','memoria': num, 'limites':None, 'direccion':None}
        else:
            num = memConst()

            tablaVariables[funcionActual]['variables'][p[-1]] = {'nombreVariable':p[-1],'tipoVariable':'int','memoria': num, 'limites':None, 'direccion':None}   
            varConst[str(p[-1])] = {'nombre': p[-1], 'memoria':num}

def p_validarFloat(p):
    "validarFloat :"

    #valida si la variable ya estaba declarada previamente
    if p[-1] not in tablaVariables['global']['variables']:
        if p[-1] not in tablaVariables[funcionActual]['variables']:

            #Si no se encontro se da de alta
            #Se busca en todas las constantes declaradas anteriores para no duplicar memoria
            if str(p[-1]) in varConst:
                num = varConst[str(p[-1])]['memoria']
                tablaVariables[funcionActual]['variables'][p[-1]] = {'nombreVariable':p[-1],'tipoVariable':'float','memoria': num, 'limites':None, 'direccion':None}
            else:
                #Se asigna la memoria previa
                num = memConst()

                #Se da de alta la variable en la funcion actual
                tablaVariables[funcionActual]['variables'][p[-1]] = {'nombreVariable':p[-1],'tipoVariable':'float','memoria': num, 'limites':None, 'direccion':None}
                #Se guarda la nueva varruable y su memoria en la lista varConst
                varConst[str(p[-1])] = {'nombre': p[-1], 'memoria':num}

    #si la variable no se encontro en las variables locales pero si en las globales, se agrega en las variables locales (se ocupa para la maquina virtual)
    if funcionActual != 'global' and p[-1] not in tablaVariables[funcionActual]['variables']:
        if str(p[-1]) in varConst:
            num = varConst[str(p[-1])]['memoria']
            tablaVariables[funcionActual]['variables'][p[-1]] = {'nombreVariable':p[-1],'tipoVariable':'float','memoria': num, 'limites':None, 'direccion':None}
        else:
            num = memConst()

            tablaVariables[funcionActual]['variables'][p[-1]] = {'nombreVariable':p[-1],'tipoVariable':'float','memoria': num, 'limites':None, 'direccion':None}   
            varConst[str(p[-1])] = {'nombre': p[-1], 'memoria':num}

def p_validarChar(p):
    "validarChar :"

    #valida si la variable ya estaba declarada previamente
    if p[-1] not in tablaVariables['global']['variables']:
        if p[-1] not in tablaVariables[funcionActual]['variables']:
            
            #Si no se encontro se da de alta
            #Se busca en todas las constantes declaradas anteriores para no duplicar memoria
            if str(p[-1]) in varConst:
                num = varConst[str(p[-1])]['memoria']
                tablaVariables[funcionActual]['variables'][p[-1]] = {'nombreVariable':p[-1],'tipoVariable':'char','memoria': num, 'limites':None, 'direccion':None}
            else:
                #Se asigna la memoria previa
                num = memConst()
               
                #Se da de alta la variable en la funcion actual
                tablaVariables[funcionActual]['variables'][p[-1]] = {'nombreVariable':p[-1],'tipoVariable':'char','memoria': num, 'limites':None, 'direccion':None}
                #Se guarda la nueva varruable y su memoria en la lista varConst
                varConst[str(p[-1])] = {'nombre': p[-1], 'memoria':num}
    
    #si la variable no se encontro en las variables locales pero si en las globales, se agrega en las variables locales (se ocupa para la maquina virtual)
    if funcionActual != 'global' and p[-1] not in tablaVariables[funcionActual]['variables']:
        if str(p[-1]) in varConst:
            num = varConst[str(p[-1])]['memoria']
            tablaVariables[funcionActual]['variables'][p[-1]] = {'nombreVariable':p[-1],'tipoVariable':'char','memoria': num, 'limites':None, 'direccion':None}
        else:
            num = memConst()

            tablaVariables[funcionActual]['variables'][p[-1]] = {'nombreVariable':p[-1],'tipoVariable':'char','memoria': num, 'limites':None, 'direccion':None}   
            varConst[str(p[-1])] = {'nombre': p[-1], 'memoria':num}

#Valida que la variable este en las globales o locales
def validarVar(izq):
    if izq not in tablaVariables[funcionActual]['variables']:
        if izq not in tablaVariables['global']['variables']:
            print(izq)
            print("Error, Variable no identificada")
            sys.exit(1)
        return 'global'
    return funcionActual


#Agrega las variables temporales
def addVarTemp(tipo):

    #Valida que el tipo resultante de la operacion no se error
    if tipo == 'err':
        print("Error, Tipos de variables no convergen")
        sys.exit(1)

    #Encuentra la memoria para asignar
    num = memTemporal()

    #Se crea la temporal en la tabla de variable de la funcion actual
    tablaVariables[funcionActual]['variables'][str('#T'+str(num-9000))] = {'nombreVariable':str('#T'+str(num-9000)),'tipoVariable':tipo,'memoria': num, 'limites':None, 'direccion':None}
    #Se da de alta en la tabla de variables
    varTemp.append({'nombreVariable':str('#T'+str(num-9000)),'tipoVariable':tipo,'memoria': num, 'limites':None, 'direccion':None})
    return str('#T'+str(num-9000))

#------------------------------------------------------

#Valida que los tipos de las operaciones sean posible realizarse 
def designer(op, izq, der):

    #Tabla con los tipo de regresa las operaciones (err significa error)
    tiposVal = {'izq':  ['int'  ,'int'  ,'int' ,'float','float','float','char','char' ,'char'],
                'der':  ['int'  ,'float','char','int'  ,'float','char' ,'int' ,'float','char'], 
                '+':    ['int'  ,'float','err' ,'float','float','err'  ,'err' ,'err'  ,'char'],
                '-':    ['int'  ,'float','err' ,'float','float','err'  ,'err' ,'err'  ,'err' ],
                '*':    ['int'  ,'float','err' ,'float','float','err'  ,'err' ,'err'  ,'err' ],
                '/':    ['float','float','err' ,'float','float','err'  ,'err' ,'err'  ,'err' ],
                '=':    ['int'  ,'err'  ,'err' ,'float','float','err'  ,'err' ,'err'  ,'char']}
    
    #Siemore regresan bool
    if op == '!=' or op == '==':
        return 'bool'

    if op == '<' or op == '>' or op == '<=' or op == '>=':
        #No se pueden comparar los char con un int o float
        if (izq == 'char' and der != 'char') or (izq != 'char' and der == 'char'):
            return 'err'
        if izq == 'bool' or der =='bool':
            return 'err'
        return 'bool'
    
    #Las dos variables tienen que ser tipos booleanas afuerza
    if (op == '&' or op =='|') and (izq != 'bool' or der != 'bool'):
        return 'err'
    elif op == '&' or op =='|':
        return 'bool'

    #regresa el valor de la tabla de tiposVal
    for i in range(len(tiposVal['izq'])):
        if tiposVal['izq'][i] == izq and tiposVal['der'][i] == der:
            return tiposVal[str(op)][i]
    
    return 'err'

#Encontrar errores a la hora de desarrollar
def p_debugErrors(p):
    "debugErrors :"

    print("AQUI SI ESTA ENTRANDO")


def p_error(p):
    print('ERROR')
    if p:
        print("Error de sintaxis en '%s'" % p.value)
    else:
        print("Error de sintaxis en EOF")
    
    sys.exit(1)


parser = yacc.yacc()




#Lee el archivo que se va a compilar
if (len(sys.argv) > 1):
    programName = sys.argv[1]
    programFile = open(programName, "r")
    program = programFile.read().replace('\\n', '\n')
    parser.parse(program)
    programFile.close()  
else:
    raise Exception('''
    No se encontro el nombre de ningun archivo
    ''')