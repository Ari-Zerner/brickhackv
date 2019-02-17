import { Opinion } from './opinion';

export class Debate {
	id:number;
	title:string;
	imageUrl:string;
	subtitle:string;
	description:string;
	viewCount:number;
	opinionCount:number;
	bookmarked:boolean;
	opined:boolean;
	voted:boolean;
	opinions:Opinion[];
	myOpinion:Opinion;
}
