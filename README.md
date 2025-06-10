# App3 - El Bosque de las Runas Mágicas 🌲✨

Este proyecto consiste en una aplicación desarrollada en **Haskell** que calcula el camino óptimo en un bosque mágico (una matriz de enteros), maximizando la energía final del mago. La lógica utiliza un algoritmo de búsqueda **A*** para encontrar la solución de manera eficiente.

La aplicación se puede ejecutar directamente desde la consola o a través de una interfaz gráfica amigable construida en **Python + Tkinter**.

---

## 🔧 Requisitos

### Opción 1: Ejecutar solo por consola

- **GHC (Glasgow Haskell Compiler)** instalado (versión 9+).
- El paquete `containers`, que generalmente viene incluido con la instalación de GHC. No se requieren librerías externas adicionales.

### Opción 2: Usar la interfaz gráfica (Python GUI)

- **Python 3.10+** instalado.
- La librería `tkinter`, que usualmente viene incluida en la instalación estándar de Python.
- Haber compilado `App3.hs` y tener el ejecutable `App3.exe` en la misma carpeta que el archivo `gui_bosque.py`.

---

## 📦 Archivos del proyecto

- `App3.hs` — Código fuente de la lógica principal en Haskell.
- `App3.exe` — Ejecutable de la aplicación (generado tras la compilación).
- `gui_bosque.py` — Código de la interfaz gráfica en Python.
- `README.md` — Este archivo.

---

## 🛠️ Compilar App3.hs

Para compilar el programa y generar el archivo `App3.exe`, abre una terminal en la carpeta del proyecto y ejecuta el siguiente comando:

```bash
ghc App3.hs -package containers -o App3.exe

## ▶️ Ejecutar por consola

Una vez compilado, puedes ejecutar el programa directamente desde la terminal con el siguiente formato:

```bash
App3.exe "[[matriz_json]]" energia_inicial
O bien con Powershell

```bash
.\App3.exe "[[matriz_json]]" energia_inicial

# Matriz 3x3
.\App3.exe "[[1,0,-1],[2,3,4],[0,-2,5]]" 5

# Matriz 6x6
.\App3.exe "[[2,-3,1,0,2,3],[-5,4,-2,1,0,-4],[1,3,0,-3,2,2],[2,-1,4,0,-5,1],[0,2,-3,3,4,1],[1,0,2,-2,1,5]]" 12

## 🖼️ Ejecutar con Interfaz Gráfica (GUI)

La interfaz gráfica ofrece una manera más cómoda de interactuar con el programa.

1.  Asegúrate de haber compilado `App3.hs` y de que `App3.exe` se encuentre en la **misma carpeta** que `gui_bosque.py`.
2.  Ejecuta el script de Python:
    ```bash
    python gui_bosque.py
    ```
Desde la GUI podrás ingresar la matriz, la energía inicial, y ver el camino resultante animado sobre la grilla.

## ⚠️ Notas Importantes

- **Rendimiento:** Para asegurar la compatibilidad y evitar los errores de compilación con librerías externas, la cola de prioridad del algoritmo A* se implementó usando una **lista ordenada** estándar de Haskell. Esto hace que el programa sea robusto, pero puede ser lento en matrices muy grandes (6x6 o superior) debido al costo de reordenar la lista en cada paso. ¡La demora es esperada!

- **Matrices Válidas:** Asegúrate de que las matrices ingresadas sean siempre cuadradas (mismo número de filas y columnas), de lo contrario el comportamiento es indefinido.

---

## 👨‍💻 Autor

Desarrollado como parte del proyecto App 3 para el curso de **Lenguajes y Paradigmas de Programación - 2025 - UAI**.
