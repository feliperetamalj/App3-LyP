# App3 - El Bosque de las Runas Mágicas 🌲✨

Este proyecto consiste en una aplicación desarrollada en **Haskell** que permite calcular el mejor camino en un bosque mágico (una matriz de enteros), maximizando la energía final. Se puede ejecutar desde consola o con una interfaz gráfica construida en **Python + Tkinter**, que permite introducir la matriz de forma más amigable.

---

## 🔧 Requisitos

### Opción 1: Solo usar por consola (App3.exe)

- [GHC (Glasgow Haskell Compiler)](https://www.haskell.org/ghc/) instalado (versión 9+)
- Módulo adicional: `heap`  
  Instálalo con:
  ```bash
  cabal update
  cabal install heap --lib
  ```

### Opción 2: Usar interfaz gráfica (Python GUI)

- [Python 3.10+](https://www.python.org/)
- Librerías necesarias:
  ```bash
  pip install tk
  ```

- Asegúrate de tener el ejecutable `App3.exe` **compilado** en la misma carpeta del archivo `gui_bosque.py`.

---

## 📦 Archivos del proyecto

- `App3.hs` — Código fuente en Haskell
- `App3.exe` — Ejecutable (compilado a partir de `App3.hs`)
- `gui_bosque.py` — Interfaz gráfica en Python
- `README.md` — Este archivo 😄

---

## 🛠️ Compilar App3.hs

Una vez tengas instalado GHC y la librería `heap`, compila así:

```bash
ghc App3.hs -package heap -o App3.exe
```

Este comando genera `App3.exe`.

---

## ▶️ Ejecutar por consola

Formato:

```bash
App3.exe "[[matriz]]" energia_inicial
```

Ejemplo (3x3):

```bash
App3.exe "[[1,0,-1],[2,3,4],[0,-2,5]]" 5
```

Ejemplo (4x4):

```bash
App3.exe "[[2,-1,0,3],[1,4,-2,0],[0,2,1,-3],[3,0,2,1]]" 8
```

Ejemplo (5x5):

```bash
App3.exe "[[1,0,2,-1,3],[2,3,0,4,-2],[1,-3,2,0,1],[0,2,3,-2,2],[3,1,0,2,4]]" 10
```

---

## 🖼️ Ejecutar GUI (opcional)

1. Asegúrate de tener `App3.exe` compilado en la **misma carpeta** que `gui_bosque.py`.
2. Luego, ejecuta:

```bash
python gui_bosque.py
```

Desde la interfaz gráfica podrás:

- Ingresar la matriz como texto (`[[...]]`)
- Elegir la energía inicial
- Calcular el mejor camino
- Ver el resultado visualmente con coordenadas y energía restante

---

## ⚠️ Notas

- Evita ingresar matrices no cuadradas: todas deben tener igual número de filas y columnas.
- Si usas la GUI y ves que se "congela", asegúrate de estar usando la **versión optimizada** del ejecutable (`App3.hs` actualizado con `Data.Heap`).
- Puedes adaptar fácilmente el programa para usarlo desde una página web u otro lenguaje si expones la lógica con un servidor.

---

## 👨‍💻 Autor

Desarrollado como parte del proyecto App 3 para el curso de **Lenguajes y Paradigmas de Programación - 2024**.
