#include <proto/exec.h>
#include <hardware/custom.h>

// misc copper stuff

// define copper commands in array(SHORT)
#define COPPERLIST_MOVE(reg, data)  offsetof(struct Custom, reg), (data)
#define COPPERLIST_WAIT(x, y, mask) (((y)<<8)|((x)<<1)|1), ((USHORT)mask) & 0xFFFE
#define COPPERLIST_SKIP(x, y, mask) (((y)<<8)|((x)<<1)|1), ((USHORT)mask) | 0x0001

// write command to the copperlist
#define COPPERLIST_SET_MOVE(copPtr, reg, data)  {*(copPtr) = offsetof(struct Custom, reg); *(copPtr+1) = ((USHORT)data); }
#define COPPERLIST_SET_MOVE_LONG(copPtr, reg, data)  {*(copPtr) = offsetof(struct Custom, reg); *(copPtr+1) = (((ULONG)data) >> 16); *(copPtr+2) = offsetof(struct Custom, reg) + 2; *(copPtr+3) = (((ULONG)data) & 0xFFFF); }
#define COPPERLIST_SET_WAIT(copPtr, x, y, mask) {*(copPtr) = (((y)<<8)|((x)<<1)|1); *(copPtr+1) = ((USHORT)mask) & 0xFFFE; }
#define COPPERLIST_SET_SKIP(copPtr, x, y, mask) {*(copPtr) = (((y)<<8)|((x)<<1)|1); *(copPtr+1) = ((USHORT)mask) | 0x0001; }

// add commands to copperlist
#define COPPERLIST_ADD_MOVE(copPtr, reg, data)  {*copPtr++ = offsetof(struct Custom, reg); *copPtr++ = ((USHORT)data); }
#define COPPERLIST_ADD_MOVE_LONG(copPtr, reg, data)  {*copPtr++ = offsetof(struct Custom, reg); *copPtr++ = (((ULONG)data) >> 16); *copPtr++ = offsetof(struct Custom, reg) + 2; *copPtr++ = (((ULONG)data) & 0xFFFF); }
#define COPPERLIST_ADD_WAIT(copPtr, x, y, mask) {*copPtr++ = (((y)<<8)|((x)<<1)|1); *copPtr++ = ((USHORT)mask) & 0xFFFE; }
#define COPPERLIST_ADD_SKIP(copPtr, x, y, mask) {*copPtr++ = (((y)<<8)|((x)<<1)|1); *copPtr++ = ((USHORT)mask) | 0x0001; }

// quick helpers
__attribute__((always_inline)) inline USHORT* copSetPlanes(UBYTE bplPtrStart,USHORT* copListEnd,const UBYTE **planes,int numPlanes) {
	for (USHORT i=0;i<numPlanes;i++) {
		ULONG addr=(ULONG)planes[i];
		*copListEnd++=offsetof(struct Custom, bplpt[0]) + (i + bplPtrStart) * sizeof(APTR);
		*copListEnd++=(UWORD)(addr>>16);
		*copListEnd++=offsetof(struct Custom, bplpt[0]) + (i + bplPtrStart) * sizeof(APTR) + 2;
		*copListEnd++=(UWORD)addr;
	}
	return copListEnd;
}

__attribute__((always_inline)) inline USHORT* copWaitXY(USHORT *copListEnd,USHORT x,USHORT i) {
	*copListEnd++=(i<<8)|(x<<1)|1;	//bit 1 means wait. waits for vertical position x<<8, first raster stop position outside the left 
	*copListEnd++=0xfffe;
	return copListEnd;
}

__attribute__((always_inline)) inline USHORT* copWaitY(USHORT* copListEnd,USHORT i) {
	*copListEnd++=(i<<8)|4|1;	//bit 1 means wait. waits for vertical position x<<8, first raster stop position outside the left 
	*copListEnd++=0xfffe;
	return copListEnd;
}

__attribute__((always_inline)) inline USHORT* copSetColor(USHORT* copListCurrent,USHORT index,USHORT color) {
	*copListCurrent++=offsetof(struct Custom, color[index]);
	*copListCurrent++=color;
	return copListCurrent;
}

