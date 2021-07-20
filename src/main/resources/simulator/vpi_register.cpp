/*
 * VPI register
 * -----------------------------------------------------------------------
 * Jimmy Situ (web@jimmystone.cn)
 */

#include <vpi_user.h>
#include <sv_vpi_user.h>


/*
 * This is a table of register functions. This table is the external
 * symbol that the simulator looks for when loading this .vpi module.
 */

extern "C" PLI_INT32 init_rsts_calltf(PLI_BYTE8 *user_data);
extern "C" PLI_INT32 init_ins_calltf(PLI_BYTE8 *user_data);
extern "C" PLI_INT32 init_outs_calltf(PLI_BYTE8 *user_data);
extern "C" PLI_INT32 init_sigs_calltf(PLI_BYTE8 *user_data);
extern "C" PLI_INT32 tick_calltf(PLI_BYTE8 *user_data);
extern "C" PLI_INT32 tick_compiletf(PLI_BYTE8 *user_data);

void init_rsts_register(void)
{
  s_vpi_systf_data tf_data;

  tf_data.type      = vpiSysTask;
  tf_data.tfname    = "$init_rsts";
  tf_data.calltf    = init_rsts_calltf;
  tf_data.sizetf    = 0;
  tf_data.compiletf = 0;
  vpi_register_systf(&tf_data);
}

void init_ins_register(void)
{
  s_vpi_systf_data tf_data;

  tf_data.type      = vpiSysTask;
  tf_data.tfname    = "$init_ins";
  tf_data.calltf    = init_ins_calltf;
  tf_data.sizetf    = 0;
  tf_data.compiletf = 0;
  vpi_register_systf(&tf_data);
}

void init_outs_register(void)
{
  s_vpi_systf_data tf_data;

  tf_data.type      = vpiSysTask;
  tf_data.tfname    = "$init_outs";
  tf_data.calltf    = init_outs_calltf;
  tf_data.sizetf    = 0;
  tf_data.compiletf = 0;
  vpi_register_systf(&tf_data);
}

void init_sigs_register(void)
{
  s_vpi_systf_data tf_data;

  tf_data.type      = vpiSysTask;
  tf_data.tfname    = "$init_sigs";
  tf_data.calltf    = init_sigs_calltf;
  tf_data.sizetf    = 0;
  tf_data.compiletf = 0;
  vpi_register_systf(&tf_data);
}

void tick_register(void)
{
  s_vpi_systf_data tf_data;

  tf_data.type      = vpiSysTask;
  tf_data.tfname    = "$tick";
  tf_data.calltf    = tick_calltf;
  tf_data.sizetf    = 0;
  tf_data.compiletf = tick_compiletf;
  vpi_register_systf(&tf_data);
}

void (*vlog_startup_routines[])(void) = {
      init_rsts_register,
      init_ins_register,
      init_outs_register,
      init_sigs_register,
      tick_register,
      0
};
