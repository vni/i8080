/*******************************************************************************
 *
 * FIXME: ADD SUPPORT FOR EQU AND SET PSEUDO OPCODES
 * FIXME: LABELS:
 * FIXME:         Should they be case sensitive or case insensitive?
 * FIXME:         Should they only be 1 to 5 characters long?
 *
 * gcc asm.c -ansi -pedantic -Wall 
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

int linenr = 1;

struct label *labels = NULL;
struct opcode opcodes[]; /* forward decl */

unsigned char object[65536];
int PC = 0; /* Current pos in program object. */
int org = 0; /* Labels refer here. */
FILE *input_stream;

/******************************************************************************/
void gen_code(unsigned char code) {
	object[PC++] = code;
	org++;
}

/******************************************************************************/
void die(const char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	fprintf(stderr, "ERROR at line %d of input, pass = %d:\n\t", linenr, pass);
	vfprintf(stderr, fmt, args);
	va_end(args);
	exit(13);
}

/* strdup is not in the ANSI C standart */
char *xstrdup(const char *str) {
	int len = strlen(str) + 1;
	char *copy = malloc(len);
	if (copy == NULL)
		die("failed to allocate memory in xstrdup\n");
	memcpy(copy, str, len);
	return copy;
}

/* PRIMITIVES FOR READING INPUT STREAM ****************************************/
static char line[256];
static int pos = 0;

/* peek current character */
int ch() {
	if (feof(input_stream))
		return EOF;
	return line[pos];
}
void nextchar() {
	char *r;

	if (feof(input_stream))
		return;

	if (line[pos] == '\0' || line[pos] == '\n') {
		/* read new line from the input stream */
		r = fgets(line, sizeof(line), input_stream);
		if (r == NULL)
			return;
		pos = 0;
	} else {
		pos++;
	}
}

/* rewind input stream for 'cnt' characters backwards */
void unget(int cnt) {
	pos -= cnt;
	if (pos < 0)
		die("ERROR: failed to unget for cnt=%d chars backwards, pos=%d is negative now\n", cnt, pos);
}

/******************************************************************************/

void skip_white() {
	while (ch() == ' ' || ch() == '\t')
		nextchar();
}

void skip_comment() {
	skip_white();
	if (ch() != ';')
		return;

	while (ch() != EOF && ch() != '\n')
		nextchar();
}

/* REGISTER NAME READING AND VALIDATION ***************************************/
/* FIXME: call it СЛУЖЕБНАЯ */
int _reg(const char **reg_names) {
	char buf[5];
	int i;

	skip_white();
	for (i=0; i<sizeof(buf)-1 && isalpha(ch()); i++) {
		buf[i] = toupper(ch());
		nextchar();
	}
	buf[i] = '\0';

	for (i=0; reg_names[i]; i++)
		if (!strcmp(buf, reg_names[i]))
			return i;
	die("Invalid register name '%s'.\n", buf);
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
	buf[0] = ch(); /* in case of first char is '@' or '?' */
	nextchar();
	for (i=1; i<sizeof(buf)-1 && isalpha(ch()); i++, nextchar())
		buf[i] = ch();
	buf[i] = '\0';

	while (l) {
		/*printf("label_value() %s\n", l->name);*/
		if (!strcmp(l->name, buf))
			return l->addr;
		l = l->next;
	}

	if (pass) { /* labels can be missed only on the first (zero) pass */
		/* SHOULDN'T BE HERE */
		die("Label '%s' is not found\n", buf);
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

	if (!isdigit(ch()))
		die("xdigit expected\n");
	for (i=0; i<sizeof(buf)-1 && isxdigit(ch()); i++, nextchar())
		buf[i] = ch();
	buf[i] = '\0';

	if (toupper(ch()) == 'H') { /* hexademical number */
		for (i=0; buf[i]; i++)
			res = (res<<4) + xdigit(buf[i]);
		nextchar();
	} else { /* decimal number */
		for (i=0; buf[i]; i++) {
			if (!isdigit(buf[i]))
				die("invalid character '%c' in decimal constant\n", ch());
			res = res*10 + (buf[i]-'0');
		}
		if (toupper(ch()) == 'D')
			nextchar();
	}

	return res;
}

int constant() {
	skip_white();
	if (ch() == '?' || ch() == '@' || isalpha(ch())) {
		return label_value();
	} else {
		int sign = 1;
		switch(ch()) {
			case '$': /* special character means 'current line number' */
				nextchar();
				return org;
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
		die("doesn't support negative exponent\n");
	while (p--)
		n *= x;
	return n;
}

int power() {
	int op1 = constant();
	skip_white();
	if (ch() == '^') {
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
		switch (ch()) {
			case '*':
				nextchar();
				op2 = power();
				op1 *= op2;
				break;
			case '/':
				nextchar();
				op2 = power();
				if (op2 == 0)
					die("division by 0\n");
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
		switch (ch()) {
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
		die("invalid value %d for imm3 operand. "
				"It should fit into 3 bits in RST instruction\n", res);
	return res;
}

int imm8() {
	int res = expr();
	if (res > 255)
		die("invalid value %d for imm8 operand. It should fit into byte\n", res);
	return res;
}

int imm16() {
	int res = expr();
	if (res > 65536)
		die("invalid value %d for imm16 operand. It should fit into word\n", res);
	return res;
}

/* OPCODE CALLBACKS ***********************************************************/
void db_cb(unsigned char unused) {
	while (1) {
		skip_white();
		if (ch() == '\'') {
			while (1) {
				nextchar();
				if (ch() == '\'') { /* check for double ' */
					nextchar();
					if (ch() == '\'')
						gen_code('\'');
					else
						break;
				} else {
					gen_code(ch());
				}
			}
		} else {
			int r = imm8();
			gen_code(r);
		}

		skip_white();
		if (ch() == ',')
			nextchar();
		else
			break;
	}
}

void dw_cb(unsigned char unused) {
	while (1) {
		int r = imm16();
		gen_code(r&0xFF);
		gen_code(r>>8);
		skip_white();
		if (ch() == ',')
			nextchar();
		else
			break;
	}
}

void ds_cb(unsigned char unused) {
	int r = imm16();
	while (r--)
		gen_code(0x00); /* just a placeholder */
}

void generic_cb(unsigned char arg) {
	gen_code(arg);
}

void inr_dcr_cb(unsigned char arg) {
	int r = reg();
	r <<= 3;
	arg |= r;
	gen_code(arg);
}

void mov_cb(unsigned char arg) {
	int r = reg();
	r <<= 3;
	arg |= r;
	skip_white();
	if (ch() != ',')
		die("MOV instruction expects ',' after the first operand, but got '%c'\n", ch());
	nextchar();
	r = reg();
	arg |= r;
	if (arg == 0x76)
		die("illegal instruction: MOV M,M\n");
	gen_code(arg);
}

void stax_ldax_cb(unsigned char arg) {
	int r = reg();
	if (r != 0 && r != 2) /* not B nor D register pair */
		die("STAX expects B or D register pair, but got %d\n", r);
	r = r ? (1<<4) : 0;
	arg |= r;
	gen_code(arg);
}

void rma_cb(unsigned char arg) {
	int r = reg();
	arg |= r;
	gen_code(arg);
}

void push_pop_cb(unsigned char arg) {
	int r = rp_psw();
	r <<= 4;
	arg |= r;
	gen_code(arg);
}

void rpi_cb(unsigned char arg) {
	int r = rp_sp();
	r <<= 4;
	arg |= r;
	gen_code(arg);
}

void lxi_cb(unsigned char arg) {
	int r = rp_sp();
	r <<= 4;
	arg |= r;
	skip_white();
	if (ch() != ',')
		die("expected operand delimiter ',', but got '%c'\n", ch());
	nextchar();
	r = imm16();
	gen_code(arg);
	gen_code(r&0xFF);
	gen_code(r>>8);
}

void mvi_cb(unsigned char arg) {
	int r = reg();
	r <<= 3;
	arg |= r;
	skip_white();
	if (ch() != ',')
		die("expected operand delimiter ',', but got '%c'\n", ch());
	nextchar();
	r = imm8();
	gen_code(arg);
	gen_code(r);
}

void imm_ins_cb(unsigned char arg) {
	int r = imm8();
	gen_code(arg);
	gen_code(r);
}

void dir_addr_ins_cb(unsigned char arg) {
	int r = imm16();
	gen_code(arg);
	gen_code(r);
	gen_code(r>>8);
}

void jmp_call_cb(unsigned char arg) {
	int r = imm16();
	gen_code(arg);
	gen_code(r);
	gen_code(r>>8);
}

void rst_cb(unsigned char arg) {
	int r = imm3();
	r <<= 3;
	arg |= r;
	gen_code(arg);
}

void in_out_cb(unsigned char arg) {
	int r = imm8();
	gen_code(arg);
	gen_code(r);
}

void org_cb(unsigned char unused) {
	org = imm16();
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
	/* PSEUDO INSTRUCTIONS */
	{ "ORG", org_cb, 0x00 },
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
	struct label *l = NULL;
	if (pass) /* create labels only on first (zero) pass */
		return;
	if (label_already_defined(lbl))
		die("label '%s' is already defined\n", lbl);
	if (!label_has_valid_name(lbl))
		die("label '%s' has invalid name\n", lbl);
	l = malloc(sizeof(struct label));
	if (l == NULL)
		die("failed to allocate memory for label node\n");
	l->name = xstrdup(lbl); 
	if (l->name == NULL) {
		free(l);
		die("failed to allocate memory for label node name\n");
	}
	l->addr = org;
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
void label() {
	char buf[6];
	int i = 0;

	skip_white();
	if (ch() == '@' || ch() == '?' || isalpha(ch())) {
		do {
			buf[i++] = ch();
			nextchar();
		} while ((i <sizeof(buf)-1) && isalpha(ch()));
		buf[i] = '\0';
		if (i == sizeof(buf)-1 && ch() != ':')
			die("OPCODE is too long\n"); 
		if (ch() == ':') {
			create_label(buf);
			nextchar(); /* skip ':' */
		} else { /* put back what we have read */
			unget(i);
		}
	}
}

void opcode() {
	char buf[6];
	int i;

	skip_white();
	if (!isalpha(ch())) {
		return;
	}
	for (i=0; i<sizeof(buf)-1 && isalpha(ch()); i++) {
		buf[i] = toupper(ch());
		nextchar();
	}
	buf[i] = '\0';

	if (i == sizeof(buf)-1)
		die("OPCODE is too long\n"); 
	if (!isspace(ch()) && ch() != EOF && ch() != ';') {
		die("OPCODE should be trailed by an empty space\n");
	}

	for (i=0; opcodes[i].mnemo; i++)
		if (strcmp(buf, opcodes[i].mnemo) == 0)
			break;
	if (opcodes[i].mnemo)
		opcodes[i].cb(opcodes[i].arg);
	else
		die("UNEXPECTED OPCODE: '%s'\n", buf);
}

void statement() {
	label();
	opcode();
	skip_comment();

	if (ch() != '\n')
		die("Expected '\\n' at the end of a statement, but got '%c' (0x%02X)\n", ch(), ch());
	nextchar(); /* skip '\n' */
	linenr++;
}

/******************************************************************************/
void process_file(const char *filepath) {
	int i;
	FILE *output_stream = NULL;
	char objfilename[32];
	char *dot;
	/* set global vars to initial state */
	line[pos=0] = '\0';
	linenr = 1;
	org = PC = 0;
	pass = 0;
	need_second_pass = 0;
	erase_labels();

	input_stream = fopen(filepath, "r");
	if (input_stream == NULL)
		die("failed to open file '%s': %s\n", filepath, strerror(errno));

	nextchar(); /* get first line */
	while (ch() != EOF)
		statement();

	if (need_second_pass) {
		rewind(input_stream);
		line[pos=0] = '\0';
		linenr = 1;
		org = PC = 0;
		pass = 1;

		nextchar();
		while (ch() != EOF)
			statement();
	}
	fclose(input_stream);

	/* write out binary image */
	dot = strrchr(filepath, '.');
	if (dot) {
		int objfilenamelen = dot - filepath + 1 + 3 + 1; /* +1 for '.', +3 for 'obj', +1 for '\0' */
		if (objfilenamelen > sizeof(objfilename))
			die("OOPS, output filename is too long: %d, should be most %d bytes long.\n",
					objfilenamelen, sizeof(objfilename)-1);
		sprintf(objfilename, "%.*s.obj", dot-filepath, filepath);
	} else {
		int objfilenamelen = strlen(filepath) + 1 + 3 + 1; /* +1 for '.', +3 for 'obj', +1 for '\0' */
		if (objfilenamelen > sizeof(objfilename))
			die("OOPS, output filename is too long: %d, should be most %d bytes long.\n",
					objfilenamelen, sizeof(objfilename)-1);
		sprintf(objfilename, "%s.obj", filepath);
	}
	output_stream = fopen(objfilename, "w");
	if (output_stream == NULL)
		die("Failed to open %s for writing: %s\n", objfilename, strerror(errno));
	/* FIXME: use fwrite instead of fprintf */
	for (i=0; i<PC; i++)
		fprintf(output_stream, "%c", object[i]);
	fclose(output_stream);
}

int main(int argc, char *argv[]) {
	while (*++argv)
		process_file(*argv);
	return 0;
}
