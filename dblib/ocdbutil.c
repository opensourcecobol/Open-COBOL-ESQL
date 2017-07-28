/*
 * Copyright (C) 2013 Tokyo System House Co.,Ltd.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * as published by the Free Software Foundation; either version 2.1,
 * or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; see the file COPYING.LIB.  If
 * not, write to the Free Software Foundation, 51 Franklin Street, Fifth Floor
 * Boston, MA 02110-1301 USA
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <malloc.h>
#include <math.h>
#include "ocdbutil.h"
#include "ocdblog.h"

char type_tc_negative_final_number[] =
{
	'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y'
//     '{', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R'
};
static int type_tc_negative_final_number_len =
	sizeof(type_tc_negative_final_number)/sizeof(type_tc_negative_final_number[0]);

/*
 * <Function name>
 *   insert_decimal_point
 *
 * <Outline>
 *   powerで指定した位置に小数点を挿入する
 *
 * <Input>
 *   @data: 挿入対象
 *   @data_size: dataに割り当てられたサイズ(バイト単位)
 *   @power: 小数点以下の桁数(負の値)
 */
void insert_decimal_point(char *data, int data_size, int power){
	int before_length, after_length;
	before_length = strlen(data);
	after_length = strlen(data) + 1;

	int n_decimal_places = -power;

	// check size of data
	if(data_size < after_length){
		return;
	} else if(n_decimal_places <= 0 || n_decimal_places >= before_length){
		return;
	}

	memmove(data + (after_length-n_decimal_places), data + (before_length-n_decimal_places),
			n_decimal_places * sizeof(char));
	data[before_length - n_decimal_places] = '.';
}

/*
 * <Function name>
 *   type_tc_is_positive
 *
 * <Outline>
 *   OCDB_TYPE_SIGNED_NUMBER_TCのデータが正負であるかを判別し、
 *   負の値の場合は符号を取り除いた数値で引数を上書きする
 *   もし該当する数値が存在しない場合は、0をセットした上でtrueを返す
 *
 * <Input>
 *   @lastchar: 判別対象の文字
 *
 * <Output>
 *   判別対象が正 : true
 *   判別対象が負 : false
 *
 */
int type_tc_is_positive(char *lastchar){
	int i;

	if(*lastchar >= '0' &&  *lastchar <= '9')
		return true;

	for(i=0; i<type_tc_negative_final_number_len; i++){
		if(*lastchar == type_tc_negative_final_number[i]){
			char tmp[2];
			sprintf(tmp, "%d", i);
			*lastchar = tmp[0];
			return false;
		}
	}

	LOG("no final_number found: %c\n", *lastchar);
	*lastchar = 0;
	return true;
}

/*
 * <Function name>
 *   ocdb_getenv
 *
 * <Outline>
 *   環境変数から値を取得する。ない場合はエラーログを残した上でNULLを返す
 *
 * <Input>
 *   @param: パラメータ名
 *   @def  : default value
 *
 * <Output>
 *   success: パラメータの値
 *   failure: default value
 */
char *ocdb_getenv(char *param, char *def){
	char *env;

	if(param == NULL){
		ERRLOG("parameter is NULL\n");
		return def;
	}

	env = getenv(param);
	if(env == NULL){
		LOG("param '%s' is not set. set default value. \n", param);
		return def;
	} else {
		LOG("param '%s' is %s. \n", param, env);
	}
	return env;
}


/*
 * <Function name>
 *   uint_to_str
 *
 * <Outline>
 *   引数として与えられた数値から文字列を生成して返す
 *
 * <Input>
 *   @i: 数値
 *
 * <Output>
 *   success: 変換された文字列
 *   failure: NULL
 */
char *
uint_to_str(int i){
	int tmp = i;
	int dig = 0;
	char *ret;

	if(i < 0) return NULL;
	do{
		dig++;
		tmp = tmp/10;
	}while(tmp > 0);

	if((ret = (char *)calloc(dig+TERMINAL_LENGTH,sizeof(char))) ==NULL){
		return NULL;
	}
	sprintf(ret,"%d",i);
	return ret;
}

/*
 * <Function name>
 *   oc_strndup
 *
 * <Outline>
 *   引数の文字列から指定文字数分を複製して返す
 *
 * <Input>
 *   @src: 入力文字列
 *   @n: 文字数
 *
 * <Output>
 *   success: 複製された文字列
 *   failure: NULL
 */
char *
oc_strndup(char *src, int n){
	char *ret;

	if(n < 0){
		return NULL;
	}
	ret = (char *)malloc(sizeof(char) * (n + 1));
	if(!src){
		return NULL;
	}

	memcpy(ret,src,n);
	ret[n] = '\0';

	return ret;
}

/*
 * <Function name>
 *   get_str_without_after_space
 *
 * <Outline>
 *   引数の文字列の後方にある空白をTRIMする
 *
 * <Input>
 *   @target: 入力文字列
 *
 * <Output>
 *   success: 変換された文字列
 *   failure: NULL
 */
char *
get_str_without_after_space(char *target){
	char *pos;

	if(!target){
		return NULL;
	}

	pos = target + strlen(target) - 1;
	for(;pos > target;pos--){
		if(*pos != ' ')
			break;
		*pos = '\0';
	}

	return target;
}

