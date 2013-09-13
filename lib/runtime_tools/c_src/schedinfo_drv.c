#include "erl_nif.h"

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include <stddef.h> /* offsetof() */
#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#include "erl_process.h"

/* NIF interface declarations */
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);

/* The NIFs: */
static ERL_NIF_TERM snoop_runq1(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] = {
    {"snoop_runq", 1, snoop_runq1}
};

ERL_NIF_INIT(schedinfo, nif_funcs, load, NULL, NULL, NULL)

static ERL_NIF_TERM enif_make_uint64_bin(ErlNifEnv* env, uint64_t value)
{
    ErlNifBinary bin;
    enif_alloc_binary(sizeof(uint64_t), &bin);
    memcpy(bin.data, &value, sizeof(uint64_t));
    return enif_make_binary(env, &bin);
}

static ERL_NIF_TERM snoop_runq1(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // id must be < erts_no_run_queues
    int id;
    ErtsRunQueue* rq;
    erts_aint32_t iflgs;
    ERL_NIF_TERM full_reds_history;
    ERL_NIF_TERM prio_info;
    int i;

    enif_get_int(env, argv[0], &id);
    rq = erts_schedid2runq((Uint) id);

    if (rq->ix == id - 1)
    {
        iflgs = erts_smp_atomic32_read_nob(&rq->info_flags);

        full_reds_history = enif_make_list(env, 0);
        for (i = ERTS_FULL_REDS_HISTORY_SIZE - 1; i >= 0; i--)
        {
            full_reds_history =
                enif_make_list_cell(env,
                                    enif_make_int(env, rq->full_reds_history[i]),
                                    full_reds_history);
        }

        prio_info = enif_make_list(env, 0);
        for (i = ERTS_NO_PROC_PRIO_LEVELS; i >= 0; i--)
        {
            prio_info =
                enif_make_list_cell(env,
                                    enif_make_tuple4(env,
                                                     enif_make_int(env, i),
                                                     enif_make_int(env, rq->procs.prio_info[i].len),
                                                     enif_make_int(env, rq->procs.prio_info[i].max_len),
                                                     enif_make_int(env, rq->procs.prio_info[i].reds)),
                                    prio_info);
        }

        return
            enif_make_list(env, 17,
                           enif_make_tuple2(env,
                                            enif_make_atom(env, "ix"),
                                            enif_make_int(env, rq->ix)),
                           enif_make_tuple2(env,
                                            enif_make_atom(env, "info_flags"),
                                            enif_make_uint(env, (unsigned int) iflgs)),
                           enif_make_tuple2(env,
                                            enif_make_atom(env, "scheduler.virtual_reds"),
                                            enif_make_int(env, rq->scheduler->virtual_reds)),
                           enif_make_tuple2(env,
                                            enif_make_atom(env, "scheduler.cpu_id"),
                                            enif_make_int(env, rq->scheduler->cpu_id)),
                           enif_make_tuple2(env,
                                            enif_make_atom(env, "waiting"),
                                            enif_make_int(env, rq->waiting)),
                           enif_make_tuple2(env,
                                            enif_make_atom(env, "woken"),
                                            enif_make_int(env, rq->woken)),
                           enif_make_tuple2(env,
                                            enif_make_atom(env, "flags"),
                                            enif_make_int(env, rq->flags)),
                           enif_make_tuple2(env,
                                            enif_make_atom(env, "check_balance_reds"),
                                            enif_make_int(env, rq->check_balance_reds)),
                           enif_make_tuple2(env,
                                            enif_make_atom(env, "full_reds_history_sum"),
                                            enif_make_int(env, rq->full_reds_history_sum)),
                           enif_make_tuple2(env,
                                            enif_make_atom(env, "full_reds_history"),
                                            full_reds_history),
                           enif_make_tuple2(env,
                                            enif_make_atom(env, "max_len"),
                                            enif_make_int(env, rq->max_len)),
                           enif_make_tuple2(env,
                                            enif_make_atom(env, "len"),
                                            enif_make_int(env, rq->len)),
                           enif_make_tuple2(env,
                                            enif_make_atom(env, "wakeup_other"),
                                            enif_make_int(env, rq->wakeup_other)),
                           enif_make_tuple2(env,
                                            enif_make_atom(env, "wakeup_other_reds"),
                                            enif_make_int(env, rq->wakeup_other_reds)),
                           enif_make_tuple2(env,
                                            enif_make_atom(env, "halt_in_progress"),
                                            enif_make_int(env, rq->halt_in_progress)),
                           enif_make_tuple2(env,
                                            enif_make_atom(env, "procs.len"),
                                            enif_make_int(env, rq->procs.len)),
                           enif_make_tuple2(env,
                                            enif_make_atom(env, "procs.prio_info"),
                                            prio_info));
    }
    else
    {
        return enif_make_badarg(env);
    }
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

