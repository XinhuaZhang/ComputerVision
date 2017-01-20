#ifndef _CAFFEDATA_H
#define _CAFFEDATA_H

#ifdef __cplusplus
extern "C" {
#endif

void openDatabase(char * dbType, char * path);
void closeDatabase();
void saveData(int width, int height, int channel,int batch, int offset, unsigned char ** data, int *label);

#ifdef __cplusplus
}
#endif

#endif /* _CAFFEDATA_H */

