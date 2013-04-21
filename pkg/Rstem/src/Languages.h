
extern SN_env *french_create_env(void);
extern void french_close_env(SN_env*);
extern int french_stem(struct SN_env *);
extern SN_env *english_create_env(void);
extern void english_close_env(SN_env*);
extern int english_stem(struct SN_env *);
extern SN_env *spanish_create_env(void);
extern void spanish_close_env(SN_env*);
extern int spanish_stem(struct SN_env *);
extern SN_env *portuguese_create_env(void);
extern void portuguese_close_env(SN_env*);
extern int portuguese_stem(struct SN_env *);
extern SN_env *german_create_env(void);
extern void german_close_env(SN_env*);
extern int german_stem(struct SN_env *);
extern SN_env *dutch_create_env(void);
extern void dutch_close_env(SN_env*);
extern int dutch_stem(struct SN_env *);
extern SN_env *swedish_create_env(void);
extern void swedish_close_env(SN_env*);
extern int swedish_stem(struct SN_env *);
extern SN_env *norwegian_create_env(void);
extern void norwegian_close_env(SN_env*);
extern int norwegian_stem(struct SN_env *);
extern SN_env *danish_create_env(void);
extern void danish_close_env(SN_env*);
extern int danish_stem(struct SN_env *);
extern SN_env *russian_create_env(void);
extern void russian_close_env(SN_env*);
extern int russian_stem(struct SN_env *);
extern SN_env *finnish_create_env(void);
extern void finnish_close_env(SN_env*);
extern int finnish_stem(struct SN_env *);
StemLanguage languages[] = {

{"french", &french_create_env, &french_close_env, &french_stem},
{"english", &english_create_env, &english_close_env, &english_stem},
{"spanish", &spanish_create_env, &spanish_close_env, &spanish_stem},
{"portuguese", &portuguese_create_env, &portuguese_close_env, &portuguese_stem},
{"german", &german_create_env, &german_close_env, &german_stem},
{"dutch", &dutch_create_env, &dutch_close_env, &dutch_stem},
{"swedish", &swedish_create_env, &swedish_close_env, &swedish_stem},
{"norwegian", &norwegian_create_env, &norwegian_close_env, &norwegian_stem},
{"danish", &danish_create_env, &danish_close_env, &danish_stem},
{"russian", &russian_create_env, &russian_close_env, &russian_stem},
{"finnish", &finnish_create_env, &finnish_close_env, &finnish_stem},
};
