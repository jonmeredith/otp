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
static ERL_NIF_TERM set_check_balance_reds2(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM schedulers_state0(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
static ERL_NIF_TERM active_schedulers(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

static ErlNifFunc nif_funcs[] = {
    {"snoop_runq", 1, snoop_runq1},
    {"set_check_balance_reds", 2, set_check_balance_reds2},
    {"schedulers_state", 0, schedulers_state0},
    {"active_schedulers", 0, active_schedulers}
};

ERL_NIF_INIT(schedinfo, nif_funcs, load, NULL, NULL, NULL)

static ERL_NIF_TERM check_runq_flg(ErlNifEnv* env, ERL_NIF_TERM acc,
                                   erts_aint32_t* iflgs_ptr, erts_aint32_t flag, const char* flagname)
{
    if ((*iflgs_ptr & flag) == flag)
    {
        *iflgs_ptr ^= ~flag; // turn off the flag bits
        return enif_make_list_cell(env,
                                   enif_make_atom(env, flagname),
                                   acc);
    }
    else
    {
        return acc;
    }
}

struct
{
    erts_aint32_t flag;
    const char* name;
}
runq_flags[] =
{
    { ERTS_RUNQ_FLG_OUT_OF_WORK, "oow" },
    { ERTS_RUNQ_FLG_HALFTIME_OUT_OF_WORK, "htoow"},
    { ERTS_RUNQ_FLG_SUSPENDED, "susp" },
    { ERTS_RUNQ_FLG_CHK_CPU_BIND, "chkcpubind" },
    { ERTS_RUNQ_FLG_INACTIVE, "inactive"},
    { ERTS_RUNQ_FLG_EMIGRATE(0), "emigrate0" },
    { ERTS_RUNQ_FLG_EMIGRATE(1), "emigrate1" },
    { ERTS_RUNQ_FLG_EMIGRATE(2), "emigrate2" },
    { ERTS_RUNQ_FLG_EMIGRATE(3), "emigrate3" },
    { ERTS_RUNQ_FLG_IMMIGRATE(0), "immigrate0" },
    { ERTS_RUNQ_FLG_IMMIGRATE(1), "immigrate1" },
    { ERTS_RUNQ_FLG_IMMIGRATE(2), "immigrate2" },
    { ERTS_RUNQ_FLG_IMMIGRATE(3), "immigrate3" },
    { ERTS_RUNQ_FLG_EVACUATE(0), "evacuate0" },
    { ERTS_RUNQ_FLG_EVACUATE(1), "evacuate1" },
    { ERTS_RUNQ_FLG_EVACUATE(2), "evacuate2" },
    { ERTS_RUNQ_FLG_EVACUATE(3), "evacuate3" },
};
static ERL_NIF_TERM build_runq_flags(ErlNifEnv* env, erts_aint32_t flag)
{
    int size = sizeof(runq_flags) / sizeof(runq_flags[0]);
    int i;
    ERL_NIF_TERM acc = enif_make_list(env, 0);

    for (i = 0; i < size; i++)
    {
        acc = check_runq_flg(env, acc, &flag, runq_flags[i].flag, runq_flags[i].name);
    }

    if (flag != 0)
    {
        acc = enif_make_list_cell(env,
                                  enif_make_uint(env, flag),
                                  acc);
    }

    return acc;
}

static ERL_NIF_TERM snoop_runq1(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // id must be <= erts_no_run_queues
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
            enif_make_list(env, 18,
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
                                            enif_make_atom(env, "flags_list"),
                                            build_runq_flags(env, rq->flags)),
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

static ERL_NIF_TERM set_check_balance_reds2(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    // id must be <= erts_no_run_queues
    int id;
    int old;
    int reds;
    ErtsRunQueue* rq;

    enif_get_int(env, argv[0], &id);
    enif_get_int(env, argv[1], &reds);

    rq = erts_schedid2runq((Uint) id);

    if (rq->ix == id - 1)
    {
        old = rq->check_balance_reds;
        if (reds < 0)
            reds = INT_MAX;
        rq->check_balance_reds = reds;

        return enif_make_int(env, old);
    }
    else
    {
        return enif_make_badarg(env);
    }
}

static ERL_NIF_TERM schedulers_state0(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    Uint total;
    Uint online;
    Uint active;
    int yield_allowed = 0;

    if (erts_schedulers_state(&total, &online, &active, yield_allowed) == ERTS_SCHDLR_SSPND_DONE)
    {
        return enif_make_tuple3(env,
                                enif_make_uint(env, total),
                                enif_make_uint(env, online),
                                enif_make_uint(env, active));
    }
    else
    {
        return enif_make_atom(env, "sspnd_yield_restart");
    }
}


static ERL_NIF_TERM active_schedulers(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    Uint active  = erts_active_schedulers();
    return enif_make_uint(env, active);
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    return 0;
}

