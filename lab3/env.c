#include "env.h"

S_table E_base_tenv(void){
	S_table s_table = S_empty();
	S_enter(s_table,S_Symbol("int"),Ty_Int());
	S_enter(s_table,S_Symbol("string"),Ty_String());
	S_enter(s_table,S_Symbol("nil"),Ty_Nil());
	S_enter(s_table,S_Symbol("void"),Ty_Void());
	return s_table;
}

S_table E_base_venv(void){
	S_table s_table = S_empty();
 	S_enter(s_table,S_Symbol("print"),E_FunEntry(Ty_TyList(Ty_String(), NULL), Ty_Void()));
	S_enter(s_table,S_Symbol("flush"),E_FunEntry(NULL, Ty_Void()));
	S_enter(s_table,S_Symbol("getchar"),E_FunEntry(NULL, Ty_String()));
	S_enter(s_table,S_Symbol("ord"),E_FunEntry(Ty_TyList(Ty_String(), NULL), Ty_Int()));
	S_enter(s_table,S_Symbol("chr"),E_FunEntry(Ty_TyList(Ty_Int(), NULL), Ty_String()));
	S_enter(s_table,S_Symbol("size"),E_FunEntry(Ty_TyList(Ty_String(), NULL), Ty_Int()));
	S_enter(s_table,S_Symbol("substring"),E_FunEntry(Ty_TyList(Ty_String(),Ty_TyList(Ty_Int(),Ty_TyList(Ty_Int(), NULL))),Ty_String()));
	S_enter(s_table,S_Symbol("concat"),E_FunEntry(Ty_TyList(Ty_String(),Ty_TyList(Ty_String(), NULL)),Ty_String()));
	S_enter(s_table,S_Symbol("not"),E_FunEntry(Ty_TyList(Ty_Int(), NULL), Ty_Int()));
	S_enter(s_table,S_Symbol("exit"),E_FunEntry(Ty_TyList(Ty_Int(), NULL), Ty_Void()));

	return s_table;
}

E_enventry E_VarEntry(Ty_ty ty) {
	E_enventry var;
	// TODO
  	var = checked_malloc(sizeof(*var));
	var->kind = E_varEntry;
	var->u.var.ty = ty;
	return var;
}

E_enventry E_FunEntry(Ty_tyList formals, Ty_ty result) {
	E_enventry fun;
	// TODO
	fun = checked_malloc(sizeof(*fun));
	fun->kind = E_funEntry;
	fun->u.fun.formals = formals;
	fun->u.fun.result = result;
	return fun;
}