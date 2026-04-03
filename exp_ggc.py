import random as rnd
import heapq as hq
import pandas as pd

class Queue:
    def __init__(self, farr, fdep, tmax, nserv=1, arrbatch=1, depbatch=1):
        self.elements = []
        self.nserv = nserv
        self.farr = farr
        self.fdep = fdep
        self.arrbatch = int(arrbatch)
        self.depbatch = int(depbatch)
        self.tmax = tmax

        self.queue = 0
        self.busyserv = 0
        self.time = 0
        self.events = []

        self.log_time = []
        self.log_event = []
        self.log_queue = []
        self.log_busy = []
        
        self.schedule_arr()


    def schedule_arr(self):
        arr_time = self.time + self.farr()
        hq.heappush(self.events, (arr_time, 'arr'))


    def schedule_dep(self):
        dep_time = self.time + self.fdep()
        hq.heappush(self.events, (dep_time, 'dep'))


    def try_start_service(self):
        while self.busyserv < self.nserv and self.queue > 0:
            batch = min(self.depbatch, self.queue)
            self.queue -= batch
            self.busyserv += 1
            self.schedule_dep()


    def process_arr(self):
        self.schedule_arr()
        self.queue += self.arrbatch
        self.try_start_service()


    def process_dep(self):
        self.busyserv -= 1
        self.try_start_service()


    def log_state(self, event):
        self.log_time.append(self.time)
        self.log_event.append(event)
        self.log_queue.append(self.queue)
        self.log_busy.append(self.busyserv)


    def run(self):
        while self.events and self.time < self.tmax:
            self.time, ev = hq.heappop(self.events)

            if ev == 'arr':
                self.process_arr()
            else:
                self.process_dep()

            self.log_state(ev)

    def print_log(self):
        return pd.DataFrame({
            'time': self.log_time,
            'event': self.log_event,
            'queue': self.log_queue,
            'busy': self.log_busy
        })
    
    def print_metrics(self):
        if len(self.log_time) < 2:
            print("Not enough data")
            return

        total_time = self.log_time[-1]

        area_q = 0.0
        area_b = 0.0
        area_s = 0.0

        for i in range(1, len(self.log_time)):
            dt = self.log_time[i] - self.log_time[i-1]

            q = self.log_queue[i-1]
            b = self.log_busy[i-1]
            s = q + b

            area_q += q * dt
            area_b += b * dt
            area_s += s * dt

        avg_q = area_q / total_time
        avg_b = area_b / total_time
        avg_s = area_s / total_time

        n_arr = sum(1 for e in self.log_event if e == 'arr') * self.arrbatch
        n_dep = sum(1 for e in self.log_event if e == 'dep') * self.depbatch

        utilization = avg_b / self.nserv
        lmbda_eff = n_arr / total_time
        throughput = n_dep / total_time

        w = avg_s / lmbda_eff if lmbda_eff > 0 else 0
        wq = avg_q / lmbda_eff if lmbda_eff > 0 else 0

        print("=== Queue Metrics ===")
        print(f"Rho: {utilization:.4f}")
        print(f"L: {avg_s:.4f}")
        print(f"Lq: {avg_q:.4f}")
        print(f"Avg in service: {avg_b:.4f}")
        print(f"W: {w:.4f}")
        print(f"Wq: {wq:.4f}")
        print(f"Throughput: {throughput:.4f}")
        print(f"Total time: {total_time:.4f}")
        print(f"Arrivals: {n_arr}")
        print(f"Departures: {n_dep}")
    

######################################################################################
# Testing queue generator
######################################################################################

# # testing M/M/c queue
# import math

# def mmc_metrics(lmbda, mu, c):
#     rho = lmbda / (c * mu)

#     sum_terms = sum((lmbda/mu)**n / math.factorial(n) for n in range(c))
#     last_term = (lmbda/mu)**c / (math.factorial(c) * (1 - rho))
#     P0 = 1.0 / (sum_terms + last_term)
    
#     Pw = last_term * P0 # Erlang C
#     Wq = Pw / (c*mu - lmbda)
#     W = Wq + 1/mu
#     Lq = lmbda * Wq
#     L = lmbda * W 
#     return {
#         "rho_global": rho,
#         #"P0": P0,
#         "Pw": Pw,
#         "Wq": Wq,
#         "W": W,
#         "Lq": Lq,
#         "L": L
#     }

# nserv = 3
# rho = 0.5
# lmbda = 2.0
# mu = lmbda / (nserv * rho)
# farr = lambda: rnd.expovariate(lmbda)
# fdep = lambda: rnd.expovariate(mu)

# metrics = mmc_metrics(lmbda, mu, nserv)
# print("Theoretical metrics (M/M/c):")
# for k,v in metrics.items():
#     print(f"{k}: {v:.4f}")

# q = Queue(nserv=nserv, farr=farr, fdep=fdep,
#           arrbatch=1, depbatch=1, tmax=10000)
# q.run()
# q.print_metrics()
# df = q.print_log()
# print(df.head())


# testing M[k]/M/1 queue
def mkm1_metrics(lmbda_event, batch_size, mu):
    lmbda_eff = lmbda_event * batch_size
    rho = lmbda_eff / mu

    EX2 = batch_size**2
    L = (rho + (lmbda_event/mu) * EX2) / (2 * (1 - rho))

    W = L / lmbda_eff
    Wq = W - 1/mu
    Lq = lmbda_eff * Wq

    return {
        "rho_global": rho,
        "lambda_eff": lmbda_eff,
        "L": L,
        "Lq": Lq,
        "W": W,
        "Wq": Wq
    }


lmbda_val = 0.2
k_val = 5
mu_val = 2.0

metrics_mkm1 = mkm1_metrics(lmbda_val, k_val, mu_val)
print("Theoretical metrics (M[k]/M/1):")
for metric_name, v in metrics_mkm1.items():
    print(f"{metric_name}: {v:.4f}")

q = Queue(nserv=1, farr=lambda: rnd.expovariate(lmbda_val), 
          fdep=lambda: rnd.expovariate(mu_val),
          arrbatch=k_val, depbatch=1, tmax=1000000)
q.run()
q.print_metrics()


# # testing for M/G/1 queue
# lmbda = 0.8
# a, b = 0.5, 1.5
# E_S = (a + b) / 2
# Var_S = ((b - a)**2) / 12
# rho = lmbda * E_S
# Cv2 = Var_S / (E_S**2)

# lq_teo = ((1 + Cv2) / 2) * (rho**2 / (1 - rho))
# w_teo = (((1 + Cv2) / 2) * (rho / (1/E_S - lmbda))) + E_S
# l_teo = lq_teo + rho

# print("=== Teoria M/G/1 (Fórmulas da Imagem) ===")
# print(f"Rho: {rho:.4f}")
# print(f"L: {l_teo:.4f}")
# print(f"Lq: {lq_teo:.4f}")
# print(f"W: {w_teo:.4f}")

# # Execução
# q = Queue(farr=lambda: rnd.expovariate(lmbda), 
#                fdep=lambda: rnd.uniform(a, b), 
#                tmax=20000)
# q.run()
# q.print_metrics()
