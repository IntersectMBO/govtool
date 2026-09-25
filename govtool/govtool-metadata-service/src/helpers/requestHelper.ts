export function makePatch(reqObj:Record<string,any>,fields:string[]):any{
  const patchObj=makeObjSubset(reqObj,fields)
  patchObj.updated_at= new Date()
  return patchObj
}

export function makeObjSubset(reqObj:Record<string,any>,fields:string[]):any{
  const patchObj:any={}
  if(!reqObj){
    return {}
  }
  fields.forEach((f)=>{
    if(reqObj[f] !==undefined){
      patchObj[f]=reqObj[f]
    }
  })
  return patchObj
}