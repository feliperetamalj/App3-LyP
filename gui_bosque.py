import tkinter as tk
from tkinter import scrolledtext, font
import subprocess, sys, os, re, json, threading, queue

class BosqueGUI(tk.Tk):
    def __init__(self):
        super().__init__()
        self.title("El Bosque de las Runas Mágicas")
        self.configure(bg="white")
        self.result_queue = queue.Queue()
        self._build_ui()
        self.check_result_queue()

    def _build_ui(self):
        # — Título —
        f_title = font.Font(size=24, weight="bold")
        tk.Label(self, text="El Bosque de las Runas Mágicas",
                 font=f_title, fg="#6b21a8", bg="white")\
          .pack(pady=(10,20))

        # — Inputs —
        frm = tk.Frame(self, bg="white")
        frm.pack(fill="x", padx=20)

        tk.Label(frm, text="Matriz (JSON o filas línea a línea):", bg="white")\
          .grid(row=0, column=0, sticky="w")
        self.txt_matriz = scrolledtext.ScrolledText(frm, width=40, height=6, wrap="none")
        self.txt_matriz.grid(row=1, column=0, rowspan=3, pady=5)

        tk.Label(frm, text="Energía inicial:", bg="white")\
          .grid(row=0, column=1, sticky="w", padx=(20,0))
        self.ent_energia = tk.Entry(frm, width=12)
        self.ent_energia.grid(row=1, column=1, padx=(20,0), sticky="w")

        tk.Label(frm, text="Delay animación (ms):", bg="white")\
          .grid(row=2, column=1, sticky="w", padx=(20,0))
        self.ent_delay = tk.Entry(frm, width=12)
        self.ent_delay.insert(0, "200")
        self.ent_delay.grid(row=3, column=1, padx=(20,0), sticky="w")

        # — Botón —
        self.btn_run = tk.Button(self, text="Encontrar Mejor Camino",
                                 bg="#6b7280", fg="white",
                                 font=font.Font(size=14, weight="bold"),
                                 activebackground="#4b5563",
                                 command=self._on_run)
        self.btn_run.pack(fill="x", padx=20, pady=15)

        # — Mensaje de estado —
        self.lbl_msg = tk.Label(self, text="", bg="white",
                                font=font.Font(size=12))
        self.lbl_msg.pack()

        # — Cuadrícula —
        self.frm_grid = tk.Frame(self, bg="white")
        self.frm_grid.pack(pady=10)

        # — Resultados —
        frm_res = tk.Frame(self, bg="white")
        frm_res.pack(fill="x", padx=20, pady=(0,20))
        self.final_path_label = tk.Label(frm_res, text="Camino: N/A",
                                         bg="white", font=font.Font(size=12))
        self.final_path_label.pack(anchor="w")
        self.final_energy_label = tk.Label(frm_res, text="Energía Final: N/A",
                                           bg="white", font=font.Font(size=12, weight="bold"))
        self.final_energy_label.pack(anchor="w")

    def _on_run(self):
        # limpia previos
        self.lbl_msg.config(text="", fg="black")
        self.final_path_label.config(text="Camino: N/A")
        self.final_energy_label.config(text="Energía Final: N/A")
        for w in self.frm_grid.winfo_children():
            w.destroy()

        raw = self.txt_matriz.get("1.0", "end").strip()
        bosque = self._parse_bosque(raw)
        if bosque is None:
            self.lbl_msg.config(text="❌ Matriz inválida o no cuadrada.", fg="red")
            return

        try:
            energia = int(self.ent_energia.get())
        except:
            self.lbl_msg.config(text="❌ Energía inválida.", fg="red")
            return

        try:
            delay = int(self.ent_delay.get())
        except:
            delay = 200

        exe = "App3.exe" if sys.platform.startswith("win") else "./App3"
        if not os.path.exists(exe):
            self.lbl_msg.config(text=f"❌ No encontré '{exe}'.", fg="red")
            return

        # dibuja bosque
        self._draw_bosque(bosque)

        # lanza worker en hilo
        self.btn_run.config(state="disabled", text="Calculando…")
        threading.Thread(target=self._worker, args=(bosque, energia, delay), daemon=True).start()

    def _parse_bosque(self, raw):
        # prueba JSON
        try:
            cand = json.loads(raw)
            if isinstance(cand, list) and all(isinstance(r, list) for r in cand):
                bosque = cand
            else:
                raise ValueError
        except:
            bosque = []
            try:
                for line in raw.splitlines():
                    line = line.strip()
                    if not line: continue
                    parts = re.split(r'[,\s]+', line)
                    bosque.append([int(x) for x in parts if x])
            except:
                return None
        n = len(bosque)
        if n == 0 or any(len(r) != n for r in bosque):
            return None
        return bosque

    def _worker(self, bosque, energia, delay):
        exe = "App3.exe" if sys.platform.startswith("win") else "./App3"
        p = subprocess.run([exe, json.dumps(bosque), str(energia)],
                           capture_output=True, text=True)
        if p.returncode != 0:
            self.result_queue.put(('error', p.stdout.strip() or p.stderr.strip()))
            return

        out = p.stdout.strip().splitlines()
        if not out or out[0].startswith("Error") or out[0].startswith("No existe"):
            self.result_queue.put(('error', "\n".join(out)))
            return

        # parsea coordenadas y energía final
        coords = []
        for line in out[1:-1]:
            m = re.match(r'\((\d+),(\d+)\)', line)
            if m:
                coords.append((int(m.group(1)), int(m.group(2))))
        try:
            energia_final = int(out[-1].split(":")[-1].strip())
        except:
            energia_final = None

        self.result_queue.put(('success', (coords, energia_final, delay)))

    def check_result_queue(self):
        try:
            typ, payload = self.result_queue.get_nowait()
        except queue.Empty:
            self.after(100, self.check_result_queue)
            return

        # reactiva botón
        self.btn_run.config(state="normal", text="Encontrar Mejor Camino")

        if typ == 'error':
            self.lbl_msg.config(text="❌ " + payload, fg="red")
        else:
            coords, energia_final, delay = payload
            self.lbl_msg.config(text="✔ Camino encontrado", fg="green")
            self._animate_path(coords, delay)
            ruta = " → ".join(f"({r},{c})" for r,c in coords)
            self.final_path_label.config(text=f"Camino: {ruta}")
            if energia_final is not None:
                self.final_energy_label.config(text=f"Energía Final Máxima: {energia_final}")

        self.after(100, self.check_result_queue)

    def _draw_bosque(self, bosque):
        for r, row in enumerate(bosque):
            for c, val in enumerate(row):
                lbl = tk.Label(self.frm_grid, text=str(val),
                               width=4, height=2, borderwidth=1, relief="solid",
                               font=font.Font(size=10))
                if val == 0:
                    lbl.config(fg="#b91c1c")
                lbl.grid(row=r, column=c, padx=2, pady=2)

    def _animate_path(self, coords, delay):
        def step(i):
            if i >= len(coords):
                return
            r, c = coords[i]
            w = self.frm_grid.grid_slaves(row=r, column=c)[0]
            w.config(bg="#fde68a")
            self.after(delay, lambda: step(i+1))
        step(0)

if __name__ == "__main__":
    BosqueGUI().mainloop()
