
#include "semant.h"

// MACROs
#define DEBUG	// switch for logs

#ifdef DEBUG
#define LOG(format, args...) fprintf(stderr, ">> "format"\n", ##args);\
fflush(stderr);
#else
#define LOG(format, ...)
#endif

int hasSameType(Ty_ty expected, Ty_ty actual) {
	if (expected == NULL & actual == NULL)
		return 0;
	if ((expected->kind == Ty_record && actual->kind == Ty_nil)
		|| (expected->kind == Ty_nil && actual->kind == Ty_record))
		return 1;
	return expected == actual;
}

// find the actual type of Ty_ty ty
Ty_ty getTy(Ty_ty ty) {
	while (ty && ty->kind == Ty_name) {
		ty = ty->u.name.ty;
	}
	return ty;
}

struct expty expTy(Tr_exp exp, Ty_ty ty) {
	struct expty e;
	e.exp = exp;
	e.ty = ty;
	return e;
}

void SEM_transProg(A_exp exp) {
	S_table venv = E_base_venv();
	S_table tenv = E_base_tenv();
	transExp(venv, tenv, exp);
}

struct expty transVar (S_table venv, S_table tenv, A_var v) {
	LOG("Start transVar:")
	switch (v->kind) {
		case A_simpleVar: {
			E_enventry env = S_look(venv, v->u.simple);
			LOG("in transVar var=%s, entry=0x%x", S_name(v->u.simple), env)
			if (env && env->kind == E_varEntry)
				return expTy(NULL, getTy(env->u.var.ty));
			else {
				LOG("%d", env->kind)
				EM_error(v->pos, "undefined variable %d", S_name(v->u.simple));
				return expTy(NULL, Ty_Int());
			}
			break;
		}
		case A_fieldVar: {
			struct expty var = transVar(venv, tenv, v->u.field.var);
			if (var.ty->kind != Ty_record)
				EM_error(v->pos, "record var required");
			Ty_fieldList fldList = var.ty->u.record;
			while (fldList && fldList->head->name != v->u.field.sym)
				fldList = fldList->tail;
			if (!fldList) {
				EM_error(v->pos, "field %s doesn't exist", S_name(v->u.field.sym));
				return expTy(NULL, NULL);
			} else
				return expTy(NULL, getTy(fldList->head->ty));
		}
		case A_subscriptVar: {
			struct expty var = transVar(venv, tenv, v->u.subscript.var);
			struct expty exp = transExp(venv, tenv, v->u.subscript.exp);
			if (var.ty->kind != Ty_array)
				EM_error(v->pos, "array required");
			if (exp.ty->kind != Ty_int) {
				EM_error(v->pos, "int required");
				return expTy(NULL, Ty_Int());
			}
			return expTy(NULL, getTy(var.ty->u.array));
		}
	}
	return expTy(NULL, NULL);
}

struct expty transExp (S_table venv, S_table tenv, A_exp a) {
	LOG("Start an exp")
	LOG("kind = %d", a->kind)
	switch(a->kind) {
		case A_opExp: {
			LOG("begin opExp")
			A_oper oper = a->u.op.oper;
			struct expty left = transExp(venv, tenv, a->u.op.left);
			struct expty right = transExp(venv, tenv, a->u.op.right);
			if (oper == A_plusOp
				|| oper == A_minusOp
				|| oper == A_timesOp
				|| oper == A_divideOp) {
				if (left.ty->kind != Ty_int) {
					EM_error(a->u.op.left->pos, "integer required");
				}
				if (right.ty->kind != Ty_int) {
					EM_error(a->u.op.right->pos, "integer required");
				}
			} else if (oper == A_eqOp
				|| oper == A_neqOp
				|| oper == A_leOp
				|| oper == A_ltOp
				|| oper == A_gtOp
				|| oper == A_geOp){
				if (!hasSameType(left.ty, right.ty)) {
					EM_error(a->pos, "same  type required");
				}
			}
			return expTy(NULL, Ty_Int());
		}
		case A_varExp:
			return transVar(venv, tenv, a->u.var);
		case A_intExp:
			return expTy(NULL, Ty_Int());
		case A_nilExp:
			return expTy(NULL, Ty_Nil());
		case A_stringExp:
			return expTy(NULL, Ty_String());

		// test10
		case A_whileExp: {
			transExp(venv, tenv, a->u.whilee.test);
			struct expty body_ty = transExp(venv, tenv, a->u.whilee.body);
			if (body_ty.ty->kind == Ty_int)
				EM_error(a->u.whilee.body->pos, "this exp must produce no value");
			return expTy(NULL, Ty_Void());
		}

		// test11
		case A_assignExp: {
			struct expty var = transVar(venv, tenv, a->u.assign.var);
			struct expty exp = transExp(venv, tenv, a->u.assign.exp);
			if(!hasSameType(var.ty, exp.ty)) {
				EM_error(a->pos, "type mismatch");
			}
			if(a->u.assign.var->kind == A_simpleVar) {
				if(immutable && !strcmp(S_name(a->u.assign.var->u.simple), immutable)){
					EM_error(a->pos, "invalid assign to index");
					immutable = NULL;
				}
			}
			return expTy(NULL, Ty_Void());
		}
		case A_forExp: {
			struct expty lo = transExp(venv, tenv, a->u.forr.lo);
			struct expty hi = transExp(venv, tenv, a->u.forr.hi);
			if(lo.ty->kind != Ty_int) {
				EM_error(a->u.forr.lo->pos, "integer type required");
			}
			if(hi.ty->kind != Ty_int) {
				EM_error(a->u.forr.hi->pos, "integer type required");
			}
			S_enter(venv, a->u.forr.var, E_VarEntry(Ty_Int()));

			S_beginScope(venv);
			immutable = S_name(a->u.forr.var);
			transExp(venv, tenv, a->u.forr.body);
			S_endScope(venv);
			return expTy(NULL, Ty_Void());
		}

		// test14
		case A_letExp: {
			LOG("begin letExp")
			S_beginScope(venv);
			S_beginScope(tenv);
			A_decList decList = a->u.let.decs;
			// FIXME: test 17 1st decList and 3rd decList corrupts
			// solution: combine all type declaration	
			A_nametyList tail = NULL; // the first type dec
			A_decList prev = NULL;
			while (decList) {
				if (decList->head->kind == A_typeDec) {
					if (tail) {
						A_nametyList l = decList->head->u.type;
						tail->tail = l;
						while (l && l->tail != NULL) 
							l = l->tail;
						tail = l;
						// delete decList
						prev->tail = decList->tail;
						decList = prev;
					} else {
						A_nametyList l = decList->head->u.type;
						while (l && l->tail != NULL) {
							l = l->tail;
						}
						tail = l;
						// adjust this decList to head
						if (prev != NULL) {
							prev->tail = decList->tail;
							A_decList tmp_list = decList;
							decList = prev;
							tmp_list->tail = a->u.let.decs;
							a->u.let.decs = tmp_list;
						}
					}
				}
				prev = decList;
				decList = decList->tail;
			}
			// show each decList

			decList = a->u.let.decs;
			while (decList) {
				if (decList->head->kind == A_typeDec) {
					LOG("Trans Type Dec!")
				}
				transDec(venv, tenv, decList->head);
				decList = decList->tail;
			}
			LOG("In let expr, start statements")
			fflush(stderr);
			struct expty exp = transExp(venv, tenv, a->u.let.body);
			S_endScope(tenv);
			S_endScope(venv);
			return exp;
		}

		// test1
		case A_arrayExp: {
			LOG("begin arrayExp")
			Ty_ty arrTy = getTy(S_look(tenv, a->u.array.typ));
			LOG("arrTy is at 0x%x", arrTy)
			// if is array type
			if (!arrTy || arrTy->kind != Ty_array) {
				EM_error(a->pos, "%s is not an array type", S_name(a->u.array.typ));
				return expTy(NULL, Ty_Nil());
			}

			// must be int
			struct expty sizeTy = transExp(venv, tenv, a->u.array.size);
			struct expty initTy = transExp(venv, tenv, a->u.array.init);
			if (getTy(initTy.ty)->kind != getTy(arrTy->u.array)->kind)
				EM_error(a->u.array.init->pos, "type mismatch");
			return expTy(NULL, arrTy);
		}

		case A_seqExp: {
			LOG("begin seqExp")
			A_expList list = a->u.seq;
			while (list && list->head) {
				transExp(venv, tenv, list->head);
				list = list->tail;
			}
		}
	}
	return expTy(NULL, NULL);
}

// used in type declaration
// return: NULL if 1.illegal type cycle
Ty_ty trans_type(S_table tenv, S_table tmp_env, A_namety namety) {
	Ty_ty type = NULL;
	string name = S_name(namety->name);
	A_ty ty = namety->ty;
	switch (ty->kind) {
		case A_nameTy: {
			S_symbol cur_sym = NULL;
			string cur_name = NULL;
			int err = 0;
			Ty_ty head = Ty_Name(namety->name, NULL);
			Ty_ty tail = head;
			while (ty && ty->kind == A_nameTy) {
				cur_sym = ty->u.name;
				cur_name = S_name(cur_sym);
				LOG("next nameTy to look up %s", cur_name)
				if (strcmp(cur_name, "int") == 0) {
					return Ty_Int();
				}
				if (strcmp(cur_name, "string") == 0) {
					return Ty_String();
				}
				// recursive in nameTy chain
				Ty_ty tmp = head;
				while (tmp) {
					if (strcmp(cur_name, S_name(tmp->u.name.sym)) == 0) {
						// FIXME: which pos should report ?
						EM_error(namety->ty->pos, "illegal type cycle");
						return NULL;
					}
					tmp = tmp->u.name.ty;
				}
				tail->u.name.ty = Ty_Name(cur_sym, NULL);

				// iterating
				ty = TAB_look(tmp_env, cur_name);
			}
			if (ty == NULL) {
				// not in tmp_env
				// maybe in tenv
				Ty_ty ty_in_tenv = S_look(tenv, cur_sym);
				if (ty_in_tenv) { // exists
					type = ty_in_tenv;
				}
			} else {
				// encounter other types	
				type = trans_type(tenv, tmp_env, A_Namety(cur_sym, ty));
			}
			// LOG("enter %s 0x%x", name, nt->head->ty)
			// TAB_enter(tmp_env, name, nt->head->ty);
			break;
		}
		case A_recordTy: {
			A_fieldList a_fieldList = ty->u.record;
			Ty_fieldList ty_fieldList = NULL;
			Ty_fieldList tmp = NULL;
			S_symbol name = a_fieldList->head->name;
			S_symbol typ = a_fieldList->head->typ;
			bool escape = a_fieldList->head->escape;
			int error = 0;
			if (a_fieldList) {
				if (!escape) {
					LOG("escape=FALSE %s,%s", S_name(name), S_name(typ))
					error = 1;
					EM_error(a_fieldList->head->pos , "type %s is illegal", S_name(typ));
					goto clean;
				}
				if (strcmp("int", S_name(typ)) != 0 && strcmp("string", S_name(typ)) != 0) {
					LOG("%s's name -> 0", S_name(typ))
					a_fieldList->head->escape = 0;
				}
				A_ty a_ty = TAB_look(tmp_env, S_name(typ));
				Ty_ty ty_ty = NULL;
				if (a_ty) {
					ty_ty = trans_type(tenv, tmp_env, A_Namety(typ, a_ty));
					if (ty_ty == NULL) {
						// some error occurs
						error = 1;
						goto clean;
					}
				} else {
					// look up in tenv
					ty_ty = S_look(tenv, typ);
					if (ty_ty == NULL) {
						error = 1;
						EM_error(a_fieldList->head->pos, "type %s in undefined", S_name(typ));
						goto clean;
					}
				}
				ty_fieldList = Ty_FieldList(Ty_Field(typ, ty_ty), NULL);
				a_fieldList = a_fieldList->tail;
				tmp = ty_fieldList;
			}
			while (a_fieldList) {
				name = a_fieldList->head->name;
				typ = a_fieldList->head->typ;
				escape = a_fieldList->head->escape;

				if (!escape) {
					LOG("escape=FALSE %s,%s", S_name(name), S_name(typ))
					error = 1;
					EM_error(a_fieldList->head->pos , "type %s is illegal", S_name(typ));
					goto clean;
				}
				if (strcmp("int", S_name(typ)) != 0 && strcmp("string", S_name(typ)) != 0) {
					LOG("%s's name -> 0", S_name(typ))
					a_fieldList->head->escape = 0;
				}
				A_ty a_ty = TAB_look(tmp_env, S_name(typ));
				Ty_ty ty_ty = NULL;
				if (a_ty) {
					ty_ty = trans_type(tenv, tmp_env, A_Namety(typ, a_ty));
					if (ty_ty == NULL) {
						// some error occurs
						error = 1;
						goto clean;
					}
				} else {
					// look up in tenv
					ty_ty = S_look(tenv, typ);
					if (ty_ty == NULL) {
						error = 1;
						EM_error(a_fieldList->head->pos, "type %s in undefined", S_name(typ));
						goto clean;
					}
				}
				tmp->tail = Ty_FieldList(Ty_Field(name, ty), NULL);
				tmp = tmp->tail;

				a_fieldList = a_fieldList->tail;
			}

			// // read each A_field and append Ty_field to Ty_fieldList
			// Ty_fieldList list = NULL;
			// Ty_fieldList tmp;
			// A_fieldList a_list = a->u.record;
			// S_symbol name;
			// S_symbol typ;
			// if (a_list) {
			// 	name = a_list->head->name;
			// 	typ = a_list->head->typ;
			// 	Ty_ty ty = S_look(tenv, typ);
			// 	list = Ty_FieldList(Ty_Field(name, ty), NULL);
			// 	a_list = a_list->tail;
			// 	tmp = list;
			// }
			// while (a_list) {
			// 	name = a_list->head->name;
			// 	typ = a_list->head->typ;
			// 	Ty_ty ty = S_look(tenv, typ);
			// 	tmp->tail = Ty_FieldList(Ty_Field(name, ty), NULL);
			// 	tmp = tmp->tail;
			// 	a_list = a_list->tail;
			// }
		clean:
			// clean each field's escape flag
			a_fieldList = ty->u.record;
			while (a_fieldList) {
				a_fieldList->head->escape = 1;
				a_fieldList = a_fieldList->tail;
			}

			if (error == 1) {
				type = NULL;
			}
			break;
		}
		case A_arrayTy: {
			S_symbol arr = ty->u.array;
			// FIXME: what if "type arr = array of type"
			A_ty arr_aty = TAB_look(tmp_env, S_name(arr));
			Ty_ty arr_ty = S_look(tenv, arr);
			if (arr_aty){
				Ty_ty tmp = trans_type(tenv, tmp_env, A_Namety(arr, arr_aty));
				if (tmp)
					type = Ty_Array(tmp);
			} else if (arr_ty != NULL) {
				type = Ty_Array(arr_ty);
			} else {
				EM_error(ty->pos, "type %s undefined", S_name(arr));
			}
			break;
		}
	}

	return type;
}

void         transDec (S_table venv, S_table tenv, A_dec d) {
	switch(d->kind) {
		case A_varDec: {
			S_symbol var = d->u.var.var;
			S_symbol typ = d->u.var.typ;
			A_exp init = d->u.var.init;
			bool escape = d->u.var.escape;
			LOG("var=%s, and typ=0x%x", S_name(var), S_name(typ))

			// for the case var a := ()
			if (!init) {
				LOG("init empty")
				S_enter(venv, var, E_VarEntry(Ty_Void()));
				break;
			}

			// typ can be empty
			// e.g.: var a: int := 1
			// 		 var a := 1
			Ty_ty t = getTy(S_look(tenv, typ));
			// t= getTy(S_look(tenv, typ));
			LOG("after lookup: type=0x%d", t)

			/**
			 * var a (:typ) := `init_expr`
			 * if typ is empty, get the correct type of init and set to 'a'
			 * else, compare two types
			 */

			// TODO: should ensure transExp() will not return NULL 
			struct expty exp_ty = transExp(venv, tenv, init);
			LOG("after transExp: type=0x%d", exp_ty.ty)
			Ty_print(exp_ty.ty);
			printf("\n");
			fflush(stdout);
			// not specify type and initial value
			if (!t && exp_ty.ty->kind == Ty_nil) {
				EM_error(d->pos, "type required");
			}
			exp_ty.ty = getTy(exp_ty.ty);
			// compare two types
			if (t && t->kind != exp_ty.ty->kind) {
				// FIXME: maybe some counter-examples
				// test44: nil is allowed to be assigned to record
				if (!(t->kind == Ty_record && exp_ty.ty->kind == Ty_nil)) {
					EM_error(init->pos, "type mismatch");
					return;
				}
			}
			switch(exp_ty.ty->kind) {
				case Ty_record: {
					string initName = S_name(init->u.record.typ);
					string typeName = S_name(typ);
					if (typeName[0] != '\0' && strcmp(initName, typeName) != 0) {
						EM_error(init->pos, "type mismatch");
					}
					break;
				}
				case Ty_array: {
					string initName = S_name(init->u.array.typ);
					string typeName = S_name(typ);
					// TODO: compare typ and init.typ	
					LOG("init=%s, type=%s", initName, typeName)
				}
			}
			E_enventry entry = E_VarEntry(t);
			S_enter(venv, d->u.var.var, entry);
			break;
		}
		case A_typeDec: {
			/**
			 * error detecting
			 * should not declare same types in one let expr
			 * should not recursively declare types
			 */

			// judge typ is a true type or a type name 
			// if it is a true type, find if it has been defined and
			// 	  enter this type into tmp_env_table
			// if it is a name type, find if it has been defined and 
			//    look up its type recursively in tmp_env_table 
			//    if value = key, it means mutually recursively decl
			//    has occured.
			// if it is a record type, also need to check

			// FIXME: but, what to do with tenv???
			// I employ the scheme that every namety maps to a real(!name)
			//     type in tenv, so there is no mutually recursive prob
			//     in let-exp's of different level

			S_table tmp_env = S_empty();
			A_nametyList nt = d->u.type;
			// step 1: check duplicate, insert each unique definition into tmp table
			while (nt) {
				string name = S_name(nt->head->name);
				LOG("type dec: name = %s", name)
				A_ty empty = TAB_look(tmp_env, name);
				if (empty) {
					EM_error(empty->pos, "two types has same name");
					// need to delete this name_ty in list ?
				} else {
					TAB_enter(tmp_env, name, nt->head->ty);
				}
				nt = nt->tail;
			}
			// step 2: check mutually recursive definition and empty definition
			nt = d->u.type;
			while (nt) {
				Ty_ty type = trans_type(tenv, tmp_env, nt->head);
				if (type == NULL) {
					// error happens
					LOG("error in parsing the type of %s", S_name(nt->head->name))
					return;
				}
				LOG("Enter a type")
				Ty_print(type);
				printf("\n");
				fflush(stdout);
				S_enter(tenv, nt->head->name, type);

				nt = nt->tail;
			}
			break;
		}
		case A_functionDec: {
			// A_fundecList funList = d->u.function;
			// A_fundec prev = NULL;
			// while (funList) {
			// 	A_fundec f = funList->head;
			// 	if (strcmp("", S_name(f->result)) == 0)
			// 		f->result = S_Symbol("void");
			// 	Ty_tyList formalTys = makeFormalTyList(tenv, f->params);
			// 	if (prev && !strcmp(S_name(funList->head->name), S_name(prev->name))) {
			// 		// test 39
			// 		// EM_error
			// 	}
			// }
			break;
		}
	}
}

Ty_ty transTy  (S_table tenv, A_ty a) {
	switch (a->kind) {
		case A_nameTy: {
			return Ty_Name(a->u.name, S_look(tenv, a->u.name));
		}
		case A_recordTy: {
			// read each A_field and append Ty_field to Ty_fieldList
			Ty_fieldList list = NULL;
			Ty_fieldList tmp;
			A_fieldList a_list = a->u.record;
			S_symbol name;
			S_symbol typ;
			if (a_list) {
				name = a_list->head->name;
				typ = a_list->head->typ;
				Ty_ty ty = S_look(tenv, typ);
				list = Ty_FieldList(Ty_Field(name, ty), NULL);
				a_list = a_list->tail;
				tmp = list;
			}
			while (a_list) {
				name = a_list->head->name;
				typ = a_list->head->typ;
				Ty_ty ty = S_look(tenv, typ);
				tmp->tail = Ty_FieldList(Ty_Field(name, ty), NULL);
				tmp = tmp->tail;
				a_list = a_list->tail;
			}
			return Ty_Record(list);
		}
		case A_arrayTy: {
			return Ty_Array(S_look(tenv, a->u.array));
		}
	}
	return NULL;
}