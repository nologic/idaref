BEGIN { 
	start=0;
    first_instruction_emitted = 0;	
	printf("BEGIN TRANSACTION;\nCREATE TABLE instructions(platform TEXT, mnem TEXT, description TEXT);");
}
{
	
	if (start == 1) {
		secondfield=$0
		start=0

		if (firstfield ~ /^[A-Z0-9.*]+$/) {
		} else {
			tmp=firstfield
			firstfield=secondfield
			secondfield=tmp
		}
		if (first_instruction_emitted) {
			printf("');\n");
		}
		printf("INSERT INTO \"instructions\" VALUES('xtensa', '%s', '%s\n", firstfield, secondfield);
		first_instruction_emitted=1
	} else {
		if ($0 ~ /\f/) {
			start = 1;
			sub(/\f/,"", $0);
			firstfield=$0
		}  else {
			print $0
		}
	}
	
}       
END {
	printf("');\nCOMMIT;\n");
}
