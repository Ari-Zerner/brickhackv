import { async, ComponentFixture, TestBed } from '@angular/core/testing';

import { DebatePreviewComponent } from './debate-preview.component';

describe('DebatePreviewComponent', () => {
  let component: DebatePreviewComponent;
  let fixture: ComponentFixture<DebatePreviewComponent>;

  beforeEach(async(() => {
    TestBed.configureTestingModule({
      declarations: [ DebatePreviewComponent ]
    })
    .compileComponents();
  }));

  beforeEach(() => {
    fixture = TestBed.createComponent(DebatePreviewComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
