import { NextFunction } from "express";
import { Request, Response } from 'express';

export default class AppError extends Error {
  public statusCode: number;

  constructor(message: string, statusCode: number=500) {
      super(message);
      this.statusCode = statusCode;
      this.name = 'AppError';
  }
}

// Wrapper function to handle both sync and async errors
export const handlerWrapper = (fn: any) => 
  (req: Request, res: Response, next: NextFunction) => {
      try {
          const result = fn(req, res, next);
          if( result instanceof Promise){
            result.catch(e=>{
              next(e)
            })
          }
      } catch (error) {
          // Handle sync errors
          next(error);
      }
  };

export const errorHandler = (err: any, req: Request, res: Response, next: NextFunction) => {
  // Default to 500 if statusCode is not set
  let statusCode = 500;
  let message = 'Internal Server Error';

  // Check if error is an instance of AppError
  if (err instanceof AppError) {
      statusCode = err.statusCode;
      message = err.message;
  } else  {
    let e=err as any
    let errname :string= e?.constructor?.name || ''
    if(errname.includes('Prisma')){
      console.error(errname,req.url,err?.message)
      message="Database error : "+errname

    }else{
      console.error("Unexpected Handler Error:",errname,req.url,err)
      message="Unexpected Handler Error:"+errname

    }
  }

  // Send JSON response with error details
  res.status(statusCode).json({
      status: 'error',
      statusCode,
      message
  });
};