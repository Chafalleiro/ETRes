# ETResPackage - Gestión de archivos de recursos de Windows (.res)

Este paquete proporciona un conjunto de unidades y componentes visuales para leer, editar, importar y exportar archivos de recursos de Windows (.res). Incluye una API funcional (uResOperations) que puede usarse directamente en aplicaciones de consola o con interfaz, así como dos componentes visuales para Lazarus/Typhon: TETResExplorer (visor) y TETResEdit (editor).

## Contenido del paquete

### Archivos fuente

- `uResDefs.pas` – Definiciones básicas (tipos, registros, constantes).
- `uResFile.pas` – Funciones de bajo nivel para leer/parsear archivos .res.
- `uResManager.pas` – Gestor de recursos en memoria.
- `uResUtils.pas` – Utilidades de reconstrucción de recursos (iconos, bitmaps, textos).
- `uMimeDetect.pas` – Detector de tipos MIME a partir de streams.
- `uResFileIO.pas` – Exportación/importación de recursos individuales.
- `uResOperations.pas` – **API unificada** para todas las operaciones. Es la puerta de entrada principal para usar la funcionalidad sin GUI.
- `uAddRes.pas` / `.frm` – Formulario de importación de recursos (usado por los componentes).
- `ETResControls.pas` – Componentes visuales TETResExplorer y TETResEdit.
- `ETResDsgn.pas` – Editores de propiedades en tiempo de diseño (opcional).

## API de funciones (uResOperations)

La unidad `uResOperations` expone todas las operaciones sobre archivos .res de forma sencilla y documentada. A continuación se resumen las funciones más importantes. Para más detalles, consulta los comentarios en el código fuente.

### Inicialización y archivos

- `ResOp_Initialize: Boolean` – Inicializa el sistema (debe llamarse una vez al inicio).
- `ResOp_Shutdown: Boolean` – Libera recursos y cierra archivos.
- `ResOp_OpenFile(const FileName: string): Boolean` – Abre un archivo .res y carga sus recursos.
- `ResOp_CloseFile: Boolean` – Cierra el archivo actual sin guardar.
- `ResOp_SaveFile(const FileName: string = ''): Boolean` – Guarda los cambios en el archivo indicado (si se omite, guarda en el mismo archivo abierto).
- `ResOp_GetFileName: string` – Devuelve el nombre del archivo actual.

### Consultas

- `ResOp_GetResourceCount: Integer` – Número de recursos.
- `ResOp_GetAllResources: TResListArray` – Lista de todos los recursos.
- `ResOp_GetTextResources: TResListArray` – Solo recursos de texto.
- `ResOp_GetImageResources: TResListArray` – Solo recursos de imagen.
- `ResOp_FindResource(const Name: string; const ResType: TResType): Integer` – Busca un recurso por nombre y tipo.
- `ResOp_GetResourceInfo(Index: Integer; out Info: TResInfo): Boolean` – Información detallada de un recurso.

### Operaciones de edición

- `ResOp_LoadResource(const Params: array of const): TMultiResLoadResult` – Carga recursos con control total (igual que `ResMgr_LoadResource`).
- `ResOp_RenameResource(Index: Integer; const NewName: string; AutoSave: Boolean = False): Boolean` – Cambia el nombre.
- `ResOp_ChangeResourceType(Index: Integer; const NewType: TResType; AutoSave: Boolean = False): Boolean` – Cambia el tipo.
- `ResOp_DeleteResource(Index: Integer; AutoSave: Boolean = False): Boolean` – Elimina un recurso.
- `ResOp_AddResource(const Name: string; const ResType: TResType; Data: TMemoryStream; LanguageID: Word = 0; AutoSave: Boolean = False): Integer` – Añade un nuevo recurso desde un stream.
- `ResOp_ImportResource(const FileName: string; const ResourceName: string; const ResType: TResType; LanguageID: Word = 0; AutoSave: Boolean = False): Boolean` – Importa un recurso desde un archivo externo.
- `ResOp_ExportResource(const ResourceName, ResTypeName: string; Param: Integer): Boolean` – Exporta un recurso a archivo (el parámetro `Param` controla los diálogos: 0=todos, 2=sin nombre, 4=silencioso).

### Utilidades

- `ResOp_GetLastError: string` – Último mensaje de error.
- `ResOp_TypeFromInt(Value: Integer): TResType` – Crea un TResType a partir de un entero.
- `ResOp_TypeFromString(const Value: string): TResType` – Crea un TResType a partir de un string.
- `ResOp_TypeToString(const ResType: TResType): string` – Representación del tipo.
- `ResOp_ValidateResourceName(const Name: string; const ResType: TResType): Boolean` – Verifica si el nombre es válido para el tipo.
- `ResOp_FindFreeOrdinalName(const BaseName: string; ResTypeInt: Integer): string` – Encuentra un nombre ordinal no usado.

## Componentes visuales

### TETResExplorer

Explorador de recursos con vista de árbol y opciones de exportación rápida.

**Propiedades principales:**
- `FileName: string` – Archivo .res a mostrar (con editor en tiempo de diseño).

**Métodos:**
- `procedure LoadFromFile(const AFileName: string)` – Carga un archivo.
- `procedure Refresh` – Recarga la lista desde el archivo abierto.
- `function GetSelectedResource: Integer` – Devuelve el índice del recurso seleccionado, o -1.

**Eventos (puedes añadirlos si lo deseas):**
- `OnResourceSelected` – Se dispara al seleccionar un recurso.
- `OnExport` – Antes/durante la exportación.

### TETResEdit

Hereda de `TETResExplorer` y añade capacidades de edición: renombrar, cambiar tipo, importar, eliminar, añadir.

**Métodos adicionales:**
- `procedure SaveToFile(const FileName: string)` – Guarda los cambios en el archivo.
- `procedure OpenFileDialog` – Abre un diálogo para seleccionar archivo.

**Nota:** En tiempo de diseño, al asignar la propiedad `FileName`, el componente intentará cargar el archivo automáticamente. Si el archivo no existe, simplemente se queda vacío.

## Ejemplos de uso

### Sin GUI, solo API

```pascal
program TestRes;

uses
  uResOperations;

var
  ResList: TResListArray;
  I: Integer;
begin
  if not ResOp_Initialize then
  begin
    WriteLn('Error initializing');
    Exit;
  end;

  if ResOp_OpenFile('miarchivo.res') then
  begin
    ResList := ResOp_GetAllResources;
    for I := 0 to High(ResList) do
      WriteLn(ResList[I].Name, ' (', ResList[I].DisplayType, ') ', ResList[I].Size, ' bytes');
    ResOp_CloseFile;
  end
  else
    WriteLn('Error: ', ResOp_GetLastError);

  ResOp_Shutdown;
end.
```

### Con componente visual
1. Coloca un TETResExplorer en un formulario.

2. En el Inspector de Objetos, haz clic en el botón "..." de la propiedad FileName y selecciona un archivo .res.

3. El árbol se llenará automáticamente con los recursos.

4. Para exportar, haz clic derecho sobre un recurso y elige "Export Quick" o "Export As...".

### Requisitos
* Free Pascal 3.0+ / Lazarus 2.0+ o Typhon.

* Paquete virtualtreeview_package (VirtualTrees) instalado en el IDE.

### Instalación
1. Abre el paquete ETResPackage.lpk en Typhon/Lazarus.

2. Compila el paquete.

3. Instala el paquete (reconstruye el IDE si es necesario).

4. Los componentes aparecerán en la paleta "ET Controls".

### Notas adicionales
Los archivos .res pueden contener hasta 65535 recursos. El paquete maneja cualquier cantidad razonable.

Los recursos de iconos y cursores se manejan correctamente, incluyendo grupos.

La detección MIME se basa en el contenido del stream, por lo que es fiable incluso sin extensión de archivo.


### Más sobre los archivos .res

Su flexibilidad y rapidez de acceso los hace muy cñómodos para incluir ficheros y datos sin necesidad de acceder a bases de datos, compresores, archivadores o el sistema de archivos del sistema operativo.

No están comprimidos ni encriptados, pero al ser básicamente un contenedor de archivos, podemos incluir archivos cifrados y comprimidos si necesitamos que un recurso no se legible fuera de nustra aplicación o control. 

Actualmente los archivos res han evolucionado desde las primeras versiones para windows de 16 bits para albergar más opciones, pero siguen teniendo básicamente la misma estructura: **Un archivo secuencial con registros de tamaño fijo y campos de longitud variable.**

+ El tamaño del registro es **siempre** 32 bit (DWORD)
+ Los campos se dividen en longitud fija para datos concretos como idioma, tamaño, desplazamiento o versión y longitud variable para campos como el nombre, el tipo de recurso o el recurso en sí. Están ordenados en cabecera y contenido, la cabecera tiene un orden determinado de campos, al final de esta comienza el recurso.
+ Cuando el campo es variable se leerán sus registros secuencialmente hasta encontrar una marca de final (byte nulo o cero) o que leamos la marca de un cambpo nuevo en el registro siguiente.
+ Al escribir los datos escribiremos registro a registro, rellenando con ceros la cola del campo cuando sus datos hayan terminado.

+ Hay recursos especiales que provienen de la intención de ahoorar espacio en sistemas antiguos, cuando el espacio de almacenamiento era mucho más limitado que hoy en día (disquetes, CD, discos duros por debajo del GB...) y se formatean eliminando las cabeceras para compartirlas entre los registros guardados bajo ese tipo. R_BITMAP y GROUP_* pertenecen a esta categoría. Otros que para mantener la compatibilidad de la codificación cambian los primeros bytes del recurso como TXT. Los hay que pueden ser ejecutados desde windows para guardar formularios, este programa no trata esa información.

+ El tipo MANIFEST se puede usar para guardar información relevante de la versión del archivo. Los ejecutables de LAzarus y Typhon, y si se espceifica en otros IDEs como Codeblocks pueden almacenar eas información en un xml y en el campo de versión, que Windows leerá.

+ Una documentación muy interesante sobre el formato puede encontrarse aquí:

  [Moon Soft - win32 resource file format](https://www.moon-soft.com/program/FORMAT/windows/res32.htm)

+ Dos programas muy completos para manejar recursos y todas sus características aquí:

  [Resource Hacker](https://www.angusj.com/resourcehacker/)

  [RisohEditor](https://katahiromz.web.fc2.com/re/en/)



+ Entrada de la wikipedia con enlaces actualizados a la magra información de Microsoft aquí:
  [Windows resource file](https://en.wikipedia.org/wiki/Resource_(Windows))

### ☕ Café virtual

[![Ko-fi](https://img.shields.io/badge/Invítame_a_un_café-FF5E5B?style=for-the-badge&logo=kofi&logoColor=white)](https://ko-fi.com/chafalleiro)
[![Patreon](https://img.shields.io/badge/Patrocionio-aa5E5B?style=for-the-badge&logo=patreon&logoColor=white)](https://patreon.com/u25697686?utm_medium=unknown&utm_source=join_link&utm_campaign=creatorshare_creator&utm_content=copyLink)
[![PayPal](https://img.shields.io/badge/Donar_PayPal-00457C?style=for-the-badge&logo=paypal&logoColor=white)](https://www.paypal.com/donate/?hosted_button_id=JQKD5VZ2FCYH2)

### 💎 Criptomonedas

**Ethereum / BSC / Polygon:**
[![Static Badge](https://img.shields.io/badge/chafalleiro.uni.eth-Donation?style=for-the-badge&logo=polygon)](https://app.uniswap.org/portfolio/0x62f830706b0Dd6B6753e27272A3A21ab8f4a328e)

## Licencia
Puedes usar este código libremente en tus proyectos, comerciales o no. Si haces mejoras, considera compartirlas.