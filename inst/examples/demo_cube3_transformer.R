# Трансформер для кубика 3x3x3 — сборка, обучение, решение
#
# Кубик как ПОСЛЕДОВАТЕЛЬНОСТЬ из 20 фигур, а не как плоский вектор. Плотная
# сеть видит 1200 чисел и должна сама выучить, что биты 61..120 — это одна
# фигура; трансформеру это сказано формой входа, и attention сравнивает фигуры
# друг с другом напрямую. Уголок повёрнут не сам по себе, а относительно
# соседей — это ровно то отношение, которое attention и считает.
#
# arch = "transformer" внутри собран из нового API ggmlR:
#   ggml_layer_transformer_block()      pre-LN блок целиком, одним вызовом
#   ggml_layer_positional_embedding()   иначе слот 3 неотличим от слота 7
#   ggml_layer_sequence_pooling()       свернуть 20 позиций в один вектор
#   ggml_layer_dense(time_distributed)  проекция one-hot фигуры в d_model
#
# Скрипт короткий по счёту: он доказывает, что связка учится, а не тренирует
# решатель. Для настоящего обучения поднять ITERS до сотен.
#
# Запуск:  Rscript inst/examples/demo_cube3_transformer.R [имя=значение ...]
#
# Параметры задаются в командной строке, без правки файла:
#   depth=20 iters=200 states=10000 batch=128 thresh=0.05
#   d_model=64 heads=4 blocks=3 ff=256 backend=auto seed=42
#
# Проверка связки (по умолчанию):
#   Rscript inst/examples/demo_cube3_transformer.R
# Настоящее обучение на скрембах до 20:
#   Rscript inst/examples/demo_cube3_transformer.R depth=20 iters=200 thresh=0.05

library(cayleyR)

stopifnot(requireNamespace("ggmlR", quietly = TRUE))

# ---------------------------------------------------------------------------
# Гиперпараметры
# ---------------------------------------------------------------------------

# Значения по умолчанию — быстрая проверка, что связка учится (~16 с).
defaults <- list(
  d_model = 64L,     # ширина модели; делится на heads
  heads   = 4L,
  blocks  = 3L,
  ff      = 256L,    # 4 * d_model, обычное отношение
  backend = "auto",  # "cpu" | "vulkan" | "auto"
  states  = 4000L,   # состояний на итерацию ADI
  depth   = 12L,     # глубина случайного скрембла
  iters   = 5L,
  batch   = 128L,
  thresh  = 0.5,     # loss_thresh: ниже него замороженная копия обновляется
  seed    = 42L
)

# Разбор аргументов вида имя=значение. Тип берётся у значения по умолчанию,
# поэтому depth=20 остаётся целым, а thresh=0.05 — вещественным.
opt <- defaults
for (a in commandArgs(trailingOnly = TRUE)) {
  kv <- strsplit(a, "=", fixed = TRUE)[[1L]]
  if (length(kv) != 2L) stop("аргумент должен быть вида имя=значение: ", a)
  key <- kv[[1L]]
  if (is.null(defaults[[key]])) {
    stop("неизвестный параметр: ", key, "\nдоступны: ",
         paste(names(defaults), collapse = ", "))
  }
  opt[[key]] <- if (is.character(defaults[[key]])) kv[[2L]]
                else if (is.integer(defaults[[key]])) as.integer(kv[[2L]])
                else as.numeric(kv[[2L]])
  if (!is.character(opt[[key]]) && is.na(opt[[key]]))
    stop("не число: ", a)
}

D_MODEL   <- opt$d_model
N_HEADS   <- opt$heads
N_BLOCKS  <- opt$blocks
FF_DIM    <- opt$ff
BACKEND   <- opt$backend

N_STATES  <- opt$states
MAX_DEPTH <- opt$depth
ITERS     <- opt$iters
BATCH     <- opt$batch
THRESH    <- opt$thresh

set.seed(opt$seed)

cat("параметры:",
    paste(sprintf("%s=%s", names(opt), unlist(opt)), collapse = " "), "\n\n")

# ---------------------------------------------------------------------------
# 1. Группа
# ---------------------------------------------------------------------------

# Полуобороты не берём: 12 четвертных ходов — стандартный набор для ADI.
moves <- c("U", "U'", "R", "R'", "F", "F'", "D", "D'", "L", "L'", "B", "B'")
g     <- cube_group(3, moves = moves)

# cube_piece_layout() сам отбрасывает фигуры, которых ни один ход не трогает:
# центры 3x3x3 под гранными поворотами стоят на месте, поэтому остаётся 20
# фигур — 8 углов по 3 стикера и 12 рёбер по 2, дополненных до 3 слотов.
# Здесь он вызван только чтобы напечатать форму; cube_adi_model() строит свой.
lay <- cube_piece_layout(g)
cat("группа   :", g$name, "-- состояние", g$n, "позиций,",
    length(g$moves), "ходов\n")
cat("фигур    :", lay$n_piece, "шириной", lay$width, "\n")
cat("вход     : [n,", lay$n_piece, ",",
    lay$n_piece * lay$width, "]\n\n")

# ---------------------------------------------------------------------------
# 2. Модель
# ---------------------------------------------------------------------------

# encoding = "piece" обязателен: последовательность трансформера — это фигуры.
# Стикерный путь дал бы эмбеддинг с осями [dim, seq_len], а attention читает
# первую ось как последовательность — cube_adi_model() это отвергает, а не
# собирает граф, который обучался бы вдоль неверной оси.
net <- cube_adi_model(g, arch = "transformer", encoding = "piece",
                      d_model = D_MODEL, n_heads = N_HEADS,
                      n_blocks = N_BLOCKS, ff_dim = FF_DIM,
                      backend = BACKEND)
print(net)
cat("\n")

# ---------------------------------------------------------------------------
# 3. Проверка формы до всякого обучения
# ---------------------------------------------------------------------------

sc0 <- cayleyR:::cube_adi_scramble(g$ptr, BATCH, MAX_DEPTH)
v0  <- cayleyR:::adi_value_of(net$value, sc0$states, BATCH,
                              net$arch, net$layout)
p0  <- cayleyR:::adi_policy_of(net$policy, sc0$states, BATCH,
                               net$n_moves, net$arch, net$layout)

cat("forward  : value", length(v0), "значений | policy",
    paste(dim(p0), collapse = " x "), "\n")
stopifnot(ncol(p0) == net$n_moves)
cat("сумма softmax по строке:", sprintf("%.4f", sum(p0[1L, ])), "\n\n")

# ---------------------------------------------------------------------------
# 4. Обучение
# ---------------------------------------------------------------------------

# ADI без готовых меток: цель для состояния — 1 + минимальная оценка его детей,
# и она точна там, где ребёнок уже собран. Замороженная копия даёт оценки,
# живая сеть по ним учится.
cat("обучение : ", ITERS, " итераций по ", N_STATES, " состояний\n\n", sep = "")

t0  <- Sys.time()
net <- cube_adi_train(net, iterations = ITERS, batch_states = N_STATES,
                      max_depth = MAX_DEPTH, batch_size = BATCH,
                      loss_thresh = THRESH, verbose = TRUE)
cat(sprintf("\nвремя    : %.1f с\n", as.numeric(Sys.time() - t0, units = "secs")))

# ---------------------------------------------------------------------------
# 5. Учится ли оно вообще
# ---------------------------------------------------------------------------

# Единственное доказательство — падение лосса. Граф собирается и молчит даже
# когда backward у какого-то узла отсутствует, поэтому проверяем числом.
vl   <- net$history$value_loss
drop <- (vl[[1L]] - vl[[length(vl)]]) / abs(vl[[1L]])
cat(sprintf("\nvalue loss %.4f -> %.4f  (%+.0f%%)\n",
            vl[[1L]], vl[[length(vl)]], -100 * drop))
cat(if (is.finite(drop) && drop > 0.10) "OK: связка учится\n"
    else "FAIL: лосс не падает\n")

# ---------------------------------------------------------------------------
# 6. Растёт ли оценка с глубиной перемешивания
# ---------------------------------------------------------------------------

# Отдельная проверка, потому что падающий лосс сам по себе бывает и у сети,
# выучившей одну константу. Оценка обязана расти с глубиной: чем дальше
# состояние от собранного, тем больше ходов до него.
cat("\nсредняя оценка по глубине перемешивания:\n")
probe <- unique(c(1L, 3L, 6L, 10L, 15L, 20L))
probe <- probe[probe <= MAX_DEPTH]
for (d in probe) {
  s <- cayleyR:::cube_adi_scramble(g$ptr, BATCH, d)
  v <- cayleyR:::adi_value_of(net$value, s$states, BATCH,
                              net$arch, net$layout)
  cat(sprintf("  глубина %2d : %6.2f\n", d, mean(v)))
}

# ---------------------------------------------------------------------------
# 7. Решение
# ---------------------------------------------------------------------------

# После пяти итераций сеть почти наверняка не решит ничего — спуск по такой
# оценке упирается в первый же локальный минимум. Вызов здесь чтобы показать,
# что штатный решатель работает с трансформером без единой правки: он читает
# net$arch и net$layout, а не знает, какая внутри архитектура.
cat("\nрешение (2 хода от собранного):\n")
s1  <- cayleyR:::cube_adi_scramble(g$ptr, 1L, 2L)
res <- cube_adi_solve(net, s1$states[1L, ], budget = 30L, batch_size = BATCH)
cat("  решено:", res$solved,
    if (res$solved) paste("--", paste(res$path, collapse = " ")) else "", "\n")

cat("\nГотово.\n")
