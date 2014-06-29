/*******************************************************************************
 *
 * FIXME: ADD SUPPORT FOR EQU, SET AND ORG PSEUDO OPCODES
 * FIXME: LABELS:
 * FIXME:         Should they be case insensitive?
 * FIXME:         Should they be 1 to 5 characters long?
 *
 ******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>

/* DATA TYPES DEFINITION ******************************************************/
struct label {
	char *name;
	unsigned short addr;
	struct label *next;
};

struct opcode {
	const char *mnemo;
	void (*cb)(unsigned char arg);
	unsigned char arg;
};

/* GLOBAL VARIABLES ***********************************************************/
int pass = 0; /* how much passes do we already have done */
int need_second_pass = 0; /* Do we need second pass? If no forward label use - don't need. */

int ch = ' ';
int linenr = 1;

struct label *labels = NULL;
struct opcode opcodes[]; /* forward decl */

unsigned char object[65536];
int PC = 0; /* Current pos in program object. Labels refer here. */
FILE *input_stream;

/******************************************************************************/
void gen_code(unsigned char code) {
	object[PC++] = code;
}

/******************************************************************************/
#define DBG(msg, ...) \
	printf("%s():%d: " msg, __FUNCTION__, __LINE__, ##__VA_ARGS__)

/******************************************************************************/
void die(const char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	vfprintf(stderr, fmt, args);
	va_end(args);
	exit(13);
}

/* PRIMITIVES FOR READING INPUT STREAM ****************************************/
void nextchar() {
	if (ch != EOF)
		ch = fgetc(input_stream);
}

void skip_white() {
	while (ch == ' ' || ch == '\t')
		nextchar();
}

void skip_comment() {
	if (ch != ';')
		return;

	while (ch != EOF && ch != '\n')
		nextchar();
}

/* REGISTER NAME READING AND VALIDATION ***************************************/
/* FIXME: call it СЛУЖЕБНАЯ */
int _reg(const char **reg_names) {
	char buf[5];
	int i;

	skip_white();
	for (i=0; i<sizeof(buf)-1 && isalpha(ch); i++) {
		buf[i] = toupper(ch);
		nextchar();
	}
	buf[i] = '\0';

	for (i=0; reg_names[i]; i++)
		if (!strcmp(buf, reg_names[i]))
			return i;
	die("ERROR on line %d: invalid register name '%s'.\n", linenr, buf);
	return -1; /* UNREACHABLE */
}

int reg() {
	static const char *reg_names[] = { "B", "C", "D", "E", "H", "L", "M", "A", NULL };
	return _reg(reg_names);
}

int rp_psw() {
	static const char *reg_names[] = { "B", "D", "H", "PSW", NULL };
	return _reg(reg_names);
}

int rp_sp() {
	static const char *reg_names[] = { "B", "D", "H", "SP", NULL };
	return _reg(reg_names);
}

/* EXPRESSION EVALUATION ******************************************************/
int label_value() {
	char buf[6];
	int i;
	struct label *l = labels;
	buf[0] = toupper(ch); /* in case of first char is '@' or '?' */
	nextchar();
	for (i=1; i<sizeof(buf)-1 && isalpha(ch); i++, nextchar())
		buf[i] = toupper(ch);
	buf[i] = '\0';

	while (l) {
		if (!strcmp(l->name, buf))
			return l->addr;
		l = l->next;
	}

	if (pass) { /* labels can be missed only on the first (zero) pass */
		/* SHOULDN'T BE HERE */
		die("ERROR at line %d: label '%s' not found\n", linenr, buf);
	}
	need_second_pass = 1;
	return 0x00;
}

int xdigit(int ch) {
	/* ch assumed to be a valid xdigit */
	if (ch >= '0' && ch <= '9')
		return ch-'0';
	ch = toupper(ch);
	return ch-'A'+10;
}

int integer() {
	char buf[6];
	int i;
	int res = 0;

	if (!isdigit(ch))
		die("ERROR at line %d: xdigit expected\n", linenr);
	for (i=0; i<sizeof(buf)-1 && isxdigit(ch); i++, nextchar())
		buf[i] = ch;
	buf[i] = '\0';

	if (toupper(ch) == 'H') { /* hexademical number */
		for (i=0; buf[i]; i++)
			res = (res<<4) + xdigit(buf[i]);
		nextchar();
	} else { /* decimal number */
		for (i=0; buf[i]; i++) {
			if (!isdigit(buf[i]))
				die("ERROR at line %d: invalid character '%c' in decimal constant\n", linenr, ch);
			res = res*10 + (buf[i]-'0');
		}
		if (toupper(ch) == 'D')
			nextchar();
	}

	return res;
}

int constant() {
	skip_white();
	if (ch == '?' || ch == '@' || isalpha(ch)) {
		return label_value();
	} else {
		int sign = 1;
		switch(ch) {
			case '$': /* special character means 'current line number' */
				nextchar();
				return PC;
			case '-':
				sign = -1;
				/* and fallthrough */
			case '+':
				nextchar();
		}
		return integer() * sign;
	}
}

int mpow(int x, int p) {
	int n = 1;
	if (p < 1)
		die("%s(): doesn't support negative exponent\n", __func__);
	while (p--)
		n *= x;
	return n;
}

int power() {
	int op1 = constant();
	skip_white();
	if (ch == '^') {
		nextchar();
		return mpow(op1, power());
	} else {
		return op1;
	}
}

int factor() {
	int op1, op2;
	op1 = power();
	while (1) {
		skip_white();
		switch (ch) {
			case '*':
				nextchar();
				op2 = power();
				op1 *= op2;
				break;
			case '/':
				nextchar();
				op2 = power();
				if (op2 == 0)
					die("%s: division by 0\n", __func__);
				op1 /= op2;
				break;
			case '%':
				nextchar();
				op2 = power();
				op1 %= op2;
				break;
			default:
				return op1;
		}
	}
}

int term() {
	int op1, op2;
	op1 = factor();
	while (1) {
		skip_white();
		switch (ch) {
			case '+':
				nextchar();
				op2 = factor();
				op1 += op2;
				break;
			case '-':
				nextchar();
				op2 = factor();
				op1 -= op2;
				break;
			default:
				return op1;
		}
	}
}

int expr() {
	return term();
}

/******************************************************************************/
int imm3() {
	int res = expr();
	if (res > 7)
		die("ERROR at line %d: invalid value %d for imm3 operand. "
				"It should fit into 3 bits in RST instruction\n", linenr, res);
	return res;
}

int imm8() {
	int res = expr();
	if (res > 255)
		die("ERROR at line %d: invalid value %d for imm8 operand. It should fit into byte\n", linenr, res);
	return res;
}

int imm16() {
	int res = expr();
	if (res > 65536)
		die("ERROR at line %d: invalid value %d for imm16 operand. It should fit into word\n", linenr, res);
	return res;
}

/* OPCODE CALLBACKS ***********************************************************/
void db_cb(unsigned char unused) {
	while (1) {
		skip_white();
		if (ch == '\'') {
			while (1) {
				nextchar();
				if (ch == '\'') { /* check for double ' */
					nextchar();
					if (ch == '\'')
						gen_code('\'');
						/*printf("%02X ", '\'');*/
					else
						break;
				} else {
					gen_code(ch);
					/*printf("%02X ", ch);*/
				}
			}
		} else {
			int r = imm8();
			gen_code(r);
			/*printf("%02X ", r);*/
		}

		skip_white();
		if (ch == ',')
			nextchar();
		else
			break;
	}
	/*printf("\n");*/
}

void dw_cb(unsigned char unused) {
	while (1) {
		int r = imm16();
		gen_code(r&0xFF);
		gen_code(r>>8);
		/*printf("%02X %02X\n", r&0xFF, r>>8);*/
		skip_white();
		if (ch == ',')
			nextchar();
		else
			break;
	}
}

void ds_cb(unsigned char unused) {
	int r = imm16();
	while (r--)
		gen_code(0x00); /* just a placeholder */
		/*printf("00\n");*/
}

void generic_cb(unsigned char arg) {
	gen_code(arg);
	/*printf("%02X\n", arg);*/
}

void inr_dcr_cb(unsigned char arg) {
	int r = reg();
	r <<= 3;
	arg |= r;
	/*printf("%02X\n", arg);*/
	gen_code(arg);
}

void mov_cb(unsigned char arg) {
	int r = reg();
	r <<= 3;
	arg |= r;
	skip_white();
	if (ch != ',')
		die("ERROR at line %d: MOV instruction expects ',' after the first operand, but got '%c'\n", linenr, ch);
	nextchar();
	r = reg();
	arg |= r;
	if (arg == 0x76)
		die("ERROR at line %d: illegal instruction: MOV M,M\n", linenr);
	gen_code(arg);
	/*printf("%02X\n", arg);*/
}

void stax_ldax_cb(unsigned char arg) {
	int r = reg();
	if (r != 0 && r != 2) /* not B nor D register pair */
		die("ERROR at line %d: STAX expects B or D register pair, but got %d\n", linenr, r);
	r = r ? (1<<4) : 0;
	arg |= r;
	/*printf("%02X\n", arg);*/
	gen_code(arg);
}

void rma_cb(unsigned char arg) {
	int r = reg();
	arg |= r;
	/*printf("%02X\n", arg);*/
	gen_code(arg);
}

void push_pop_cb(unsigned char arg) {
	int r = rp_psw();
	r <<= 4;
	arg |= r;
	/*printf("%02X\n", arg);*/
	gen_code(arg);
}

void rpi_cb(unsigned char arg) {
	int r = rp_sp();
	r <<= 4;
	arg |= r;
	/*printf("%02X\n", arg);*/
	gen_code(arg);
}

void lxi_cb(unsigned char arg) {
	int r = rp_sp();
	r <<= 4;
	arg |= r;
	skip_white();
	if (ch != ',')
		die("ERROR at line %d: expected operand delimiter ',', but got '%c'\n", linenr, ch);
	nextchar();
	r = imm16();
	/*printf("%02X %02X %02X\n", arg, r&0xFF, r>>8);*/
	gen_code(arg);
	gen_code(r&0xFF);
	gen_code(r>>8);
}

void mvi_cb(unsigned char arg) {
	int r = reg();
	r <<= 3;
	arg |= r;
	skip_white();
	if (ch != ',')
		die("ERROR at line %d: expected operand delimiter ',', but got '%c'\n", linenr, ch);
	nextchar();
	r = imm8();
	/*printf("%02X %02X\n", arg, r);*/
	gen_code(arg);
	gen_code(r);
}

void imm_ins_cb(unsigned char arg) {
	int r = imm8();
	/*printf("%02X %02X\n", arg, r);*/
	gen_code(arg);
	gen_code(r);
}

void dir_addr_ins_cb(unsigned char arg) {
	int r = imm16();
	/*printf("%02X %02X %02X\n", arg, r&0xFF, r>>8);*/
	gen_code(arg);
	gen_code(r);
	gen_code(r>>8);
}

void jmp_call_cb(unsigned char arg) {
	int r = imm16();
	/*printf("%02X %02X %02X\n", arg, r&0xFF, r>>8);*/
	gen_code(arg);
	gen_code(r);
	gen_code(r>>8);
}

void rst_cb(unsigned char arg) {
	int r = imm3();
	r <<= 3;
	arg |= r;
	/*printf("%02X\n", arg);*/
	gen_code(arg);
}

void in_out_cb(unsigned char arg) {
	int r = imm8();
	/*printf("%02X %02X\n", arg, r);*/
	gen_code(arg);
	gen_code(r);
}

/* OPCODE TABLE ***************************************************************/
struct opcode opcodes[] = {
	/* DB, DW AND DS */
	{ "DB", db_cb, 0x00 },
	{ "DW", dw_cb, 0x00 },
	{ "DS", ds_cb, 0x00 },
	/* CARRY BIT INSTRUCTIONS */
	{ "CMC", generic_cb, 0x3F },
	{ "STC", generic_cb, 0x37 },
	/* SINGLE REGISTER INSTRUCTIONS */
	{ "INR", inr_dcr_cb, 0x04 },
	{ "DCR", inr_dcr_cb, 0x05 },
	{ "CMA", generic_cb, 0x2F },
	{ "DAA", generic_cb, 0x27 },
	/* NOP INSTRUCTIONS */
	{ "NOP", generic_cb, 0x00 },
	/* DATA TRANSFER INSTRUCTIONS */
	{ "MOV", mov_cb, 0x40 },
	{ "STAX", stax_ldax_cb, 0x02 },
	{ "LDAX", stax_ldax_cb, 0x0A },
	/* REGISTER OR MEMORY TO ACCUMULATOR INSTRUCTIONS */
	{ "ADD", rma_cb, 0x80 },
	{ "ADC", rma_cb, 0x88 },
	{ "SUB", rma_cb, 0x90 },
	{ "SBB", rma_cb, 0x98 },
	{ "ANA", rma_cb, 0xA0 },
	{ "XRA", rma_cb, 0xA8 },
	{ "ORA", rma_cb, 0xB0 },
	{ "CMP", rma_cb, 0xB8 },
	/* ROTATE ACCUMULATOR INSTRUCTIONS */
	{ "RLC", generic_cb, 0x07 },
	{ "RRC", generic_cb, 0x0F },
	{ "RAL", generic_cb, 0x17 },
	{ "RAR", generic_cb, 0x1F },
	/* REGISTER PAIR INSTRUCTIONS */
	{ "PUSH", push_pop_cb, 0xC5 },
	{ "POP", push_pop_cb, 0xC1 },
	{ "DAD", rpi_cb, 0x09 },
	{ "INX", rpi_cb, 0x03 },
	{ "DCX", rpi_cb, 0x0B },
	{ "XCHG", generic_cb, 0xEB },
	{ "XTHL", generic_cb, 0xE3 },
	{ "SPHL", generic_cb, 0xF9 },
	/* IMMEDIATE INSTRUCTIONS */
	{ "LXI", lxi_cb, 0x01 },
	{ "MVI", mvi_cb, 0x06 },
	{ "ADI", imm_ins_cb, 0xC6 },
	{ "ACI", imm_ins_cb, 0xCE },
	{ "SUI", imm_ins_cb, 0xD6 },
	{ "SBI", imm_ins_cb, 0xDE },
	{ "ANI", imm_ins_cb, 0xE6 },
	{ "XRI", imm_ins_cb, 0xEE },
	{ "ORI", imm_ins_cb, 0xF6 },
	{ "CPI", imm_ins_cb, 0xFE },
	/* DIRECT ADDRESSING INSTRUCTIONS */
	{ "STA", dir_addr_ins_cb, 0x32 },
	{ "LDA", dir_addr_ins_cb, 0x3A },
	{ "SHLD", dir_addr_ins_cb, 0x22 },
	{ "LHLD", dir_addr_ins_cb, 0x2A },
	/* JUMP INSTRUCTIONS */
	{ "PCHL", generic_cb, 0xE9 },
	{ "JMP", jmp_call_cb, 0xC3 },
	{ "JC", jmp_call_cb, 0xDA },
	{ "JNC", jmp_call_cb, 0xD2 },
	{ "JZ", jmp_call_cb, 0xCA },
	{ "JNZ", jmp_call_cb, 0xC2 },
	{ "JM", jmp_call_cb, 0xFA },
	{ "JP", jmp_call_cb, 0xF2 },
	{ "JPE", jmp_call_cb, 0xEA },
	{ "JPO", jmp_call_cb, 0xE2 },
	/* CALL SUBROUTINE INSTRUCTIONS */
	{ "CALL", jmp_call_cb, 0xCD },
	{ "CC", jmp_call_cb, 0xDC },
	{ "CNC", jmp_call_cb, 0xD4 },
	{ "CZ", jmp_call_cb, 0xCC },
	{ "CNZ", jmp_call_cb, 0xC4 },
	{ "CM", jmp_call_cb, 0xFC },
	{ "CP", jmp_call_cb, 0xF4 },
	{ "CPE", jmp_call_cb, 0xEC },
	{ "CPO", jmp_call_cb, 0xE4 },
	/* RETURN FROM SUBROUTINE INSTRUCTIONS */
	{ "RET", generic_cb, 0xC9 },
	{ "RC", generic_cb, 0xD8 },
	{ "RNC", generic_cb, 0xD0 },
	{ "RZ", generic_cb, 0xC8 },
	{ "RNZ", generic_cb, 0xC0 },
	{ "RM", generic_cb, 0xF8 },
	{ "RP", generic_cb, 0xF0 },
	{ "RPE", generic_cb, 0xE8 },
	{ "RPO", generic_cb, 0xE0 },
	/* RST INSTRUCTIONS */
	{ "RST", rst_cb, 0xC7 },
	/* INTERRUPT FLIP-FLOP INSTRUCTIONS */
	{ "EI", generic_cb, 0xFB },
	{ "DI", generic_cb, 0xF3 },
	/* INPUT/OUTPUT INSTRUCTIONS */
	{ "IN", in_out_cb, 0xDB },
	{ "OUT", in_out_cb, 0xD3 },
	/* HLT HALT INSTRUCTION */
	{ "HLT", generic_cb, 0x76 },
	/* TERMINATOR */
	{ NULL, NULL, 0x00 },
};

/* LABEL MANIPULATION FUNCTIONS ***********************************************/
int label_has_valid_name(const char *lbl) {
	int i;
	for (i=0; opcodes[i].mnemo; i++)
		if (!strcmp(opcodes[i].mnemo, lbl))
			return 0;
	return 1;
}

int label_already_defined(const char *lbl) {
	struct label *l = labels;
	while (l) {
		if (!strcmp(l->name, lbl))
			return 1;
		l=l->next;
	}
	return 0;
}

void create_label(const char *lbl) {
	if (pass) /* create labels only on first (zero) pass */
		return;
	if (label_already_defined(lbl))
		die("ERROR at line %d: label '%s' is already defined\n", linenr, lbl);
	if (!label_has_valid_name(lbl))
		die("ERROR at line %d: label '%s' has invalid name\n", linenr, lbl);
	struct label *l = malloc(sizeof(struct label));
	if (l == NULL)
		die("ERROR: failed to allocate memory for label node\n");
	l->name = strdup(lbl);
	if (l->name == NULL)
		die("ERROR: failed to allocate memory for label node name\n");
	l->addr = PC;
	l->next = labels;
	labels = l;
}

void erase_labels(void) {
	struct label *l = labels, *t;
	while (l) {
		t = l;
		l = l->next;
		free(t->name);
		free(t);
	}
	labels = NULL;
}

/******************************************************************************/
void opcode() {
	char buf[6];
	int i, label_created = 0;

	skip_white();

	if (ch == '@' || ch == '?') {
		buf[0] = ch;
		nextchar();
		for (i=1; i<sizeof(buf)-1 && isalpha(ch); i++) {
			buf[i] = ch;
			nextchar();
		} buf[i] = '\0';
		if (ch != ':')
			die("ERROR at line %d: ':' after label '%s' is expected\n", linenr, buf);
		nextchar(); /* skip ':' */
		create_label(buf);
		label_created = 1;
		skip_white();
	} else {
		/* maybe opcode or label */
		for (i=0; i<sizeof(buf)-1 && isalpha(ch); i++) {
			buf[i] = toupper(ch);
			nextchar();
		} buf[i] = '\0';

		if (ch == ':') { /* a label */
			create_label(buf);
			nextchar(); /* skip ':' */
			label_created = 1;
			skip_white();
		}
	}

	switch (ch) {
		case EOF: return;
		case '\n': return;
		case ';': return;
	}

	if (label_created) { /* read opcode only if previously we have read label */
		for (i=0; i<sizeof(buf)-1 && isalpha(ch); i++) {
			buf[i] = toupper(ch);
			nextchar();
		} buf[i] = '\0';
	}

	if (i == sizeof(buf)-1)
		die("ERROR at line %d: OPCODE is too long\n", linenr); 
	if (!isspace(ch) && ch != EOF)
		die("ERROR at line %d: OPCODE should be trailed by an empty space\n", linenr);

	for (i=0; opcodes[i].mnemo; i++)
		if (strcmp(buf, opcodes[i].mnemo) == 0)
			break;
	if (opcodes[i].mnemo)
		opcodes[i].cb(opcodes[i].arg);
	else
		die("ERROR at line %d: UNEXPECTED OPCODE: %s\n", linenr, buf);
}

void statement() {
	opcode();

	skip_white();
	skip_comment();

	if (ch != '\n')
		die("ERROR at line %d: Expected '\\n' at the end of a statement\n", linenr);
	linenr++;
}

/******************************************************************************/
void process_file(const char *filepath) {
	/* set global vars to initial state */
	linenr = 1;
	PC = 0;
	pass = 0;
	need_second_pass = 0;
	ch = ' ';
	erase_labels();

	input_stream = fopen(filepath, "r");
	if (input_stream == NULL)
		die("ERROR: failed to open file '%s': %s\n", filepath, strerror(errno));

	while (ch != EOF) {
		statement();
		nextchar();
	}

	if (need_second_pass) {
		rewind(input_stream);
		linenr = 1;
		PC = 0;
		pass = 1;
		ch = ' ';

		while (ch != EOF) {
			statement();
			nextchar();
		}
	}
	fclose(input_stream);

	{
		int i;
		for (i=0; i<PC; i++) {
			printf(" %02X ", object[i]);
			if ((i+1) % 16 == 0)
				printf("\n");
		}
		if (i%16)
			printf("\n");
		return;
		struct label *l = labels;
		printf("\nLABELS:\n");
		while (l) {
			printf("%s: %04X\n", l->name, l->addr);
			l = l->next;
		}
	}
}

int main(int argc, char *argv[]) {
	while (*++argv)
		process_file(*argv);
	return 0;
}
